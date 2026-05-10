use aho_corasick::{AhoCorasick, MatchKind};
use serde::Serialize;
use smallvec::SmallVec;
use std::borrow::Cow;
use std::cmp::Ordering;
use fancy_regex::Regex;
use unicode_categories::UnicodeCategories;
use unicode_normalization::UnicodeNormalization;
use crate::{string_utils::*, BuiltinMacro, Node, Nodes, Options, Part, Result};

#[derive(Debug, Serialize)]
struct Entry {
    word: Node,
    #[serde(skip)] collated_word: SmallStr,
    #[serde(skip)] plain_text_word: SmallStr,
    #[serde(flatten)] data: EntryData,
}

#[derive(Debug, Serialize)]
#[serde(untagged)]
enum EntryData {
    RefEntry {
        r#ref: Node,
        #[serde(skip_serializing_if = "Option::is_none")] search: Option<String>,
    },
    FullEntry {
        /// Part of speech.
        pos: Node,

        /// Etymology.
        #[serde(skip_serializing_if = "Option::is_none")] etym: Option<Node>,

        /// IPA.
        #[serde(skip_serializing_if = "Option::is_none")] ipa: Option<Node>,

        /// Primary definition, before any sense actual sense. This is also used
        /// if there is only one sense.
        #[serde(skip_serializing_if = "Option::is_none")] primary_definition: Option<Sense>,

        /// Senses after the primary definition. If there are multiple
        /// senses, the primary definition is everything before the
        /// first slash and thus often empty.
        #[serde(skip_serializing_if = "SmallVec::is_empty")] senses: SmallVec<[Sense; 4]>,

        /// Forms. Mainly used for verbs.
        #[serde(skip_serializing_if = "Option::is_none")] forms: Option<Node>,

        /// Used for text search.
        #[serde(skip_serializing_if = "Option::is_none")] hw_search: Option<String>,
        #[serde(skip_serializing_if = "Option::is_none")] def_search: Option<String>,
    }
}

use EntryData::*;
use crate::generator::StringReplacementOp::{Lower, RemovePunct};

#[derive(Debug, Serialize)]
struct Sense {
    def: Node,
    #[serde(skip_serializing_if = "Option::is_none")] comment: Option<Node>,
    #[serde(skip_serializing_if = "Vec::is_empty")] examples: Vec<Example>,
}

#[derive(Debug, Serialize)]
struct Example {
    text: Node,
    #[serde(skip_serializing_if = "Option::is_none")] comment: Option<Node>,
}

#[derive(Debug, Default)]
pub struct Generator {
    entries: Vec<Entry>,
    opts: Options,
    ipa_converter: Vec<StringReplacementOp>,
    preprocessor: Vec<PreprocessDirective>,
    custom_macros: Vec<CustomMacroDecl>,
    collate: Option<CollateDirective>
}

// Source location info for a parser.
struct ParserContext<'s> {
    /// The entire input we're parsing. We don't use 'str' since we want to
    /// be able to index into it.
    full_input_text: &'s [u8],

    /// The offset to the start of the current thing we're parsing; used for
    /// diagnostics. Unfortunately, we can't just use the text we're parsing
    /// for this since we may perform concatenation in some cases, which then
    /// causes the text we're operating on to no longer be part of the original
    /// input text.
    loc: u32,
}

struct Parser<'s> {
    context: ParserContext<'s>,

    /// The text that we still need to parse.
    text: Stream<'s>,

    /// Generator we’re parsing into.
    g: &'s mut Generator,
}

struct TeXParser<'a> {
    context: &'a ParserContext<'a>,
    custom_macros: &'a [CustomMacroDecl],
}

type SmallStr = super::NodeText;

/// Normalisation applied before a trie.
#[derive(Debug, Copy, Clone)]
enum NormalisationForm {
    NFC,
    NFD,
    NFKC,
    NFKD,
}

impl NormalisationForm {
    fn apply(self, s: &str) -> SmallStr {
        match self {
            NormalisationForm::NFC => s.nfc().collect(),
            NormalisationForm::NFD => s.nfd().collect(),
            NormalisationForm::NFKC => s.nfkc().collect(),
            NormalisationForm::NFKD => s.nfkd().collect(),
        }
    }
}

/// Operation to convert text to IPA.
#[derive(Debug)]
enum StringReplacementOp {
    /// Contains a list of replacements that are only applied
    /// if we're converting a lemma to IPA.
    Lemma(Vec<StringReplacementOp>),

    /// Convert to lowercase.
    Lower,

    /// Normalise the input.
    Normalise(NormalisationForm),

    /// Remove punctuation marks.
    RemovePunct,

    /// Perform regex substitution.
    Subst { regex: Regex, replacement: SmallStr },

    /// Apply a trie.
    Trie {
        trie: AhoCorasick,
        normalisation: Option<NormalisationForm>,
        replacements: Vec<SmallStr>
    },
}

#[derive(Debug)]
struct PreprocessDirective {
    field: Part,
    op: PreprocessOp,
}

#[derive(Debug)]
enum PreprocessOp {
    Match { negated: bool, regex: Regex, message: Option<String> },
    Replace(StringReplacementOp)
}

#[derive(Debug)]
struct CustomMacroDecl {
    name: SmallStr,
    args: u32,
    line: u32,
}

#[derive(Debug)]
struct CollateDirective {
    by: Option<SmallVec<[char; 32]>>,
    ops: Vec<StringReplacementOp>,
    line: u32,
}

impl CollateDirective {
    fn make_default() -> Self {
        Self {
            by: None,
            line: 0,
            ops: vec![
                StringReplacementOp::Normalise(NormalisationForm::NFKD),
                RemovePunct,
                StringReplacementOp::Normalise(NormalisationForm::NFC),
                Lower,
            ]
        }
    }
}

enum SmallCow<'a> {
    Small(SmallStr),
    Std(String),
    Borrowed(&'a str),
}

impl<'a> SmallCow<'a> {
    fn as_bytes(&'a self) -> &'a [u8] {
        self.as_str().as_bytes()
    }

    fn as_str(&'a self) -> &'a str {
        match self {
            SmallCow::Small(s) => s.as_str(),
            SmallCow::Std(s) => s.as_str(),
            SmallCow::Borrowed(s) => s,
        }
    }

    fn into_small(self) -> SmallStr {
        match self {
           SmallCow::Small(s) => s,
           SmallCow::Std(s) => s.into(),
           SmallCow::Borrowed(s) => s.into(),
        }
    }
}

const SENSE_MACRO: &[u8] = b"\\\\";
const EX_MACRO: &[u8] = b"\\ex";
const COMMENT_MACRO: &[u8] = b"\\comment";

/// Normalise a string so it can be used for searching by the frontend.
fn normalise_for_search(text: &str) -> String {
    // NFKD; Latin-ASCII; [^a-z A-Z\\ ] Remove; Lower
    let text: String = text
        .nfkd()
        .filter(|c| c.is_ascii_alphabetic() || *c == ' ')
        .flat_map(|c| c.to_lowercase())
        .collect();

    // The steps below only apply to the haystack, not the needle, and should
    // NOT be applied on the frontend:
    //
    // Yeet all instances of 'sbdsth', which is what 'sbd./sth.' degenerates to.
    let text = text.replace("sbdsth", "");

    // Unique all words and sort them.
    let mut words: Vec<&str> = text.trim().split(" ").filter(|c|!c.is_empty()).collect();
    words.sort();
    words.dedup();
    words.join(" ")
}

/// Convert a byte slice to a str
fn make_str(s: &[u8]) -> &str {
    if cfg!(debug_assertions) {
        str::from_utf8(s).unwrap()
    } else {
        // Safety: All of our string parsing splits strings at ASCII characters, which
        // means that if the original string as UTF-8, so are any of the parts we split
        // off.
        unsafe { str::from_utf8_unchecked(s) }
    }
}

fn make_err(c: &ParserContext, msg: &str) -> String {
    let line = 1 + Stream::new(&c.full_input_text[..(c.loc as usize)]).count(b'\n');
    format!("Error near line {}: {}", line, msg)
}

impl<'a> ParserContext<'a> {
    fn save_offset(&mut self, text: &[u8]) {
        self.loc = self.full_input_text.element_offset(&text[0]).unwrap() as u32
    }
}

impl<'a> TeXParser<'a> {
    fn err(&self, msg: &str) -> Result<()> {
        Err(make_err(&self.context, msg))
    }

    /// Handle an unknown macro.
    fn handle_unknown_macro(
        &self,
        macro_name: &[u8],
        args: Nodes,
    ) -> Result<Node> {
        for m in self.custom_macros {
            if m.name.as_bytes() == macro_name {
                if m.args != args.len() as u32 {
                    self.err(&format!(
                        "Macro '\\{}' expects {} argument{}, but got {}",
                        m.name,
                        m.args,
                        if m.args == 1 { "" } else { "s" },
                        args.len()
                    ))?
                }

                // Ok, argument count matches.
                return Ok(Node::CustomMacro { name: m.name.clone(), args })
            }
        }

        let name = make_str(macro_name);
        Err(make_err(&self.context, &format!(
            "Unknown macro '\\{}'; did you forget to '$declare {} {}' somewhere?",
            name,
            name,
            args.len()
        )))
    }

    #[allow(unused)] // Function used by the WASM bindings.
    pub fn parse(tex: &[u8], custom_macros: &[CustomMacroDecl]) -> Result<Node> {
        let ctx = ParserContext { full_input_text: tex, loc: 0 };
        Self::parse_with_context(tex, &ctx, custom_macros)
    }

    fn parse_with_context(
        tex: &[u8],
        context: &ParserContext,
        custom_macros: &[CustomMacroDecl]
    ) -> Result<Node> {
        let p = TeXParser { context, custom_macros };
        p.parse_tex(tex)
    }

    fn parse_tex(&self, tex: &[u8]) -> Result<Node> {
        let mut s = Stream::new(tex);
        let mut nodes = Nodes::new();
        s.trim();
        while !s.is_empty() { self.parse_tex_content(&mut s, &mut nodes, 0)? }
        Ok(Node::group(nodes))
    }

    fn parse_tex_content(&self, s: &mut Stream, nodes: &mut Nodes, mut braces: i32) -> Result<()> {
        while !s.is_empty() {
            // Append to an existing text node if possible and drop empty nodes.
            fn push_node(nodes: &mut Nodes, n: Node) {
                if let Node::Text(t) = n {
                    if t.is_empty() { return }
                    if !nodes.is_empty() && let Node::Text(prev) = nodes.last_mut().unwrap() {
                        prev.push_str(&t);
                        return
                    }
                    nodes.push(Node::Text(t));
                } else {
                    nodes.push(n);
                }
            }

            let text = s.take_until_any(b"\\${}");
            push_node(nodes, Node::text(make_str(text)));

            // Process the special character.
            match s.front().unwrap_or(b'\0') {
                b'\\' => push_node(nodes, self.parse_tex_macro(s)?),
                b'$' => push_node(nodes, self.parse_tex_maths(s)?),
                b'{' => {
                    s.drop_byte();
                    braces += 1;
                },
                b'}' => {
                    s.drop_byte();
                    braces -= 1;
                    if braces == 0 { return Ok(()) }
                    if braces < 0 { return self.err("Too many '}'s!") }
                }
                _ => break,
            }
        }

        // If 'braces' is initially 0, it’s possible for us to get here without
        // ever encountering a closing brace. This happens frequently if this
        // function is invoked at the top-level of the parser.
        if braces != 0 { self.err("Unexpected end of input. Did you forget a '}}'?") }
        else { Ok(()) }
    }

    fn parse_tex_group(&self, s: &mut Stream) -> Result<Node> {
        assert!(s.consume(b"{"));
        if s.consume(b"}") { return Ok(Node::empty()) }
        let mut children = Nodes::new();
        self.parse_tex_content(s, &mut children, 1)?;
        Ok(Node::group(children))
    }

    fn parse_tex_macro(&self, s: &mut Stream) -> Result<Node> {
        assert!(s.consume(b"\\"));
        if s.is_empty() { self.err("Invalid macro escape sequence")? }

        // Soft hyphen.
        if s.consume(b"-") { return Ok(Node::builtin(BuiltinMacro::SoftHyphen)) }

        // '\\' is invalid here.
        if s.consume(b"\\") { self.err("'\\\\' cannot be used in this field")? }

        // These are treated literally.
        if s.starts_with_any(b" &$%#{}") {
            return Ok(Node::text(make_str(&[s.take_byte().unwrap()])))
        }

        // These are unsupported single-character macros.
        if s.starts_with_any(b"!/:@[]`{~") {
            return self.handle_unknown_macro(&[s.take_byte().unwrap()], vec![]);
        }

        // Handle builtin macros.
        let macro_name = s.take_while_any(b"abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ@");
        match macro_name {
            b"s" => self.parse_tex_single_arg_builtin_macro(s, BuiltinMacro::SmallCaps),
            b"w" => self.parse_tex_single_arg_builtin_macro(s, BuiltinMacro::Lemma),
            b"textit" | b"i" => self.parse_tex_single_arg_builtin_macro(s, BuiltinMacro::Italic),
            b"textbf" | b"b" => self.parse_tex_single_arg_builtin_macro(s, BuiltinMacro::Bold),
            b"textnf" => self.parse_tex_single_arg_builtin_macro(s, BuiltinMacro::Normal),
            b"senseref" => self.parse_tex_single_arg_builtin_macro(s, BuiltinMacro::Sense),
            b"Sup" => self.parse_tex_single_arg_builtin_macro(s, BuiltinMacro::Superscript),
            b"Sub" => self.parse_tex_single_arg_builtin_macro(s, BuiltinMacro::Subscript),
            b"ref" => self.parse_tex_single_arg_builtin_macro(s, BuiltinMacro::Reference),
            b"par" => Ok(Node::builtin(BuiltinMacro::ParagraphBreak)),
            b"ldots" => Ok(Node::builtin(BuiltinMacro::Ellipsis)),
            b"this" => Ok(Node::builtin(BuiltinMacro::This)),
            b"ex" | b"comment" => {
                self.err(&format!("'\\{}' cannot be used in this field", make_str(macro_name)))?;
                unreachable!();
            },
            b"label" => {
                let _ = self.parse_tex_group(s)?; // Throw away the argument.
                Ok(Node::empty())
            }
            _ => {
                let mut args = Nodes::new();
                while s.starts_with(b"{") { args.push(self.parse_tex_group(s)?); }
                self.handle_unknown_macro(macro_name, args)
            }
        }
    }

    fn parse_tex_maths(&self, s: &mut Stream) -> Result<Node> {
        assert!(s.consume(b"$"));
        let node = Node::Math(make_str(s.take_until(b"$")).into()); // TODO: Support maths properly.
        if !s.consume(b"$") { self.err("Expected '$'; if you meant to write a literal dollar sign, write '\\$'")? }
        Ok(node)
    }

    fn parse_tex_single_arg_builtin_macro(&self, s: &mut Stream, m: BuiltinMacro) -> Result<Node> {
        // Drop everything until the argument brace. We’re not a LaTeX tokeniser, so we don’t
        // support stuff like `\fract1 2`, as much as I like to write it.
        if !s.trim_start().starts_with(b"{") {
            self.err("Sorry, macro arguments must be enclosed in braces")?;
        }

        let arg = self.parse_tex_group(s)?;
        Ok(Node::builtin_with_args(m, vec![arg]))
    }
}

impl<'s> Parser<'s> {
    fn apply_string_replacement_op(s: &mut SmallCow, op: &StringReplacementOp, is_lemma: bool) {
        use StringReplacementOp::*;
        use SmallCow::*;
        match op {
            Lemma(ops) => {
                if is_lemma {
                    for op in ops {
                        Self::apply_string_replacement_op(s, op, is_lemma);
                    }
                }
            },
            Lower => *s = Std(s.as_str().to_lowercase()),
            Normalise(form) => *s = Small(form.apply(s.as_str())),
            RemovePunct => *s = Small(s.as_str().chars().filter(|c| !c.is_mark() && !c.is_punctuation()).collect()),
            Subst { regex, replacement } => {
                let rep = regex.replace_all(s.as_str(), replacement.as_str());
                if let Cow::Owned(o) = rep { *s = Std(o); }
            }
            Trie { trie, normalisation, replacements } => {
                if let Some(n) = normalisation { *s = Small(n.apply(s.as_str())) }
                *s = Std(trie.replace_all(s.as_str(), &replacements));
            }
        }
    }

    /// Check if we're looking at text.
    fn at(&self, what: &[u8]) -> bool {
        self.text.starts_with(what)
    }

    /// Helper to compile a user-provided regex.
    fn compile_regex(&self, re: &[u8]) -> Result<Regex> {
        let regex = make_str(re);
        Regex::new(regex).map_err(|e| make_err(&self.context, &format!(
            "Failed to compile regex '{}': {}",
            regex,
            e.to_string()
        )))
    }

    /// '\\ex' and friends are control macros that are really part of the dictionary
    /// syntax rather than markup; this function is used to disallow them in certain
    /// contexts.
    fn disallow_specials(&self, text: &[u8], context: &str) -> Result<()> {
        let s = Stream::new(text);
        for special in [COMMENT_MACRO, EX_MACRO, SENSE_MACRO] {
            if s.find(special).is_some() {
                return self.err(&format!(
                    "'{}' cannot be used in {}",
                    make_str(special),
                    context
                ));
            }
        }

        Ok(())
    }

    /// Check if we're done parsing.
    fn done(&self) -> bool { self.text.is_empty() }

    /// Issue a parse error.
    fn err(&self, msg: &str) -> Result<()> {
        Err(make_err(&self.context, msg))
    }

    /// Expect a string.
    fn expect(&mut self, what: &[u8]) -> Result<()> {
        if self.text.consume(what) { Ok(()) }
        else { self.err(&format!("Expected '{}'", make_str(what))) }
    }

    /// Create a new parser.
    fn new(g: &'s mut Generator, input: &'s str) -> Self {
        Self {
            context: ParserContext {
                full_input_text: input.as_bytes(),
                loc: 0,
            },
            text: Stream::new(input.as_bytes()),
            g
        }
    }

    /// Normalise a string for sorting.
    fn normalise_for_sort(&self, text: &'s str) -> SmallCow<'s> {
        match &self.g.collate {
            None => panic!("Collation not initialised!"),
            Some(coll) => {
                let mut str = SmallCow::Borrowed(text);

                // Apply preprocessing steps.
                for op in &coll.ops { Self::apply_string_replacement_op(&mut str, op, false); }

                // Delete everything not in 'by' if present,
                if let Some(by) = &coll.by {
                    str = SmallCow::Small(str
                        .as_str()
                        .chars()
                        .filter(|c| by.contains(c))
                        .collect()
                    );
                }

                str
            },
        }
    }

    /// Parse the input.
    fn parse(&mut self) -> Result<()> {
        // Parse directives.
        //
        // Directives modify how entries are processed, so if a directive is defined after
        // we've already seen some entries, it won't be applied to those; this is almost
        // certainly not what you want.
        while !self.skip_ws().done() && self.at(b"$") {
            self.context.save_offset(self.text.text());
            self.parse_directive()?;
        }

        // Get the default collation if the user didn't define one.
        if self.g.collate.is_none() { self.g.collate = Some(CollateDirective::make_default()); }

        // Parse entries.
        while !self.skip_ws().done() {
            self.context.save_offset(self.text.text());
            if self.at(b"$") { self.err("Directives must precede all entries")?; }
            self.parse_entry()?;
        }

        Ok(())
    }

    /// Parse a directive.
    ///
    /// ```ebnf
    /// <directive> ::= <dir-ipa> | <dir-preprocess> | <dir-declare> | <dir-collate>
    /// ```
    fn parse_directive(&mut self) -> Result<()> {
        assert!(self.text.starts_with(b"$"));
        self.text.consume(b"$");
        match self.text.take_until_any(b" \t\r\n{") {
            b"collate" => self.parse_collate_directive(),
            b"declare" => self.parse_declare_directive(),
            b"ipa" => self.parse_ipa_directive(),
            b"preprocess" => self.parse_preprocess_directive(),
            dir => self.err(&format!("Unrecognised directive: '{}'", make_str(dir)))
        }
    }

    /// Parse a collation directive.
    ///
    /// ```ebnf
    /// <dir-collate> ::= "$collate" "{" { <clause> } "}"
    /// <clause>      ::= <by> | <preprocess>
    /// <by>          ::= "by" <string>
    /// <preprocess>  ::= "preprocess" "{" { <string-replacement-op> } "}"
    /// ```
    fn parse_collate_directive(&mut self) -> Result<()> {
        // Parse '{'.
        self.skip_ws().expect(b"{")?;

        // Parse clauses.
        let mut by = None;
        let mut ops = Vec::new();
        while !self.skip_ws().done() && !self.at(b"}") {
            match self.text.take_until_any(b" \t\r\n{") {
                b"by" => {
                    if by.is_some() { return self.err("Duplicate 'by' in $collate directive"); }
                    self.skip_ws();
                    self.expect(b"\"")?;
                    by = Some(make_str(self.text.take_until_and_drop(b"\"")).chars().collect());
                },
                b"preprocess" => {
                    // Parse '{'.
                    self.skip_ws().expect(b"{")?;
                    while !self.skip_ws().done() && !self.at(b"}") {
                        let op = self.parse_string_replacement_op()?;
                        ops.push(op);
                    }

                    // Parse '}'.
                    self.skip_ws().expect(b"}")?;
                },
                dir => {
                    return self.err(&format!("Unknown clause in $collate: {}", make_str(dir)));
                }
            }
        }

        // Parse '}'.
        self.skip_ws().expect(b"}")?;

        // Two collation directives don't really make sense.
        if let Some(c) = &self.g.collate {
            return self.err(&format!("A $collate directive was already given on line {}", c.line))
        }

        self.g.collate = Some(CollateDirective { by, ops, line: self.context.loc });
        Ok(())
    }


    /// Parse a macro declaration directive.
    ///
    /// ```ebnf
    /// <dir-declare> ::= "$declare" <name> <number> "\n"
    /// ```
    fn parse_declare_directive(&mut self) -> Result<()> {
        let name: SmallStr = make_str(self.skip_ws().text.take_until_any(b" \t")).into();
        let args = make_str(self.skip_ws().text.take_until_any(b" \t\r\n"));
        let args = args.parse::<u32>().map_err(|e|
            make_err(
                &self.context,
                &format!("Argument count '{}' is not a valid integer: {}", args, e.to_string())
            )
        )?;

        if !self.text.take_until_any(b"\n#").trim_ascii().is_empty() {
            self.err("Extraneous junk at the end of $declare directive")?;
        }

        self.skip_ws();
        for m in &self.g.custom_macros {
            if m.name == name {
                self.err(&format!(
                    "Custom macro '{}' was already declared on line {}",
                    name,
                    m.line
                ))?;
            }
        }

        self.g.custom_macros.push(CustomMacroDecl { name, args, line: self.context.loc });
        Ok(())
    }

    /// Parse an ipa translation directive.
    ///
    /// ```ebnf
    /// <dir-ipa> ::= "$ipa" "{" { <dir-ipa-op> } "}"
    /// ```
    fn parse_ipa_directive(&mut self) -> Result<()> {
        // Parse '{'.
        self.skip_ws().expect(b"{")?;

        // Parse <op>s.
        while !self.skip_ws().done() && !self.at(b"}") {
            let op = self.parse_ipa_replacement_op()?;
            self.g.ipa_converter.push(op);
        }

        // Parse '}'.
        self.skip_ws().expect(b"}")?;
        Ok(())
    }

    /// Parse an op that can appear in an '$ipa' directive.
    ///
    /// ```ebnf
    /// <dir-ipa-op> ::= { <string-replacement-op> | <lemma-op> }
    /// <lemma-op>   ::= "lemma" "{" { <dir-ipa-op> } "}"
    /// ```
    fn parse_ipa_replacement_op(&mut self) -> Result<StringReplacementOp> {
        if self.text.consume(b"lemma") {
            // Parse '{'.
            self.skip_ws().expect(b"{")?;

            // Parse ops recursively.
            let mut ops = Vec::new();
            while !self.skip_ws().done() && !self.at(b"}") {
                let op = self.parse_ipa_replacement_op()?;
                ops.push(op);
            }

            // Parse '}'.
            self.skip_ws().expect(b"}")?;
            Ok(StringReplacementOp::Lemma(ops))
        } else {
            self.parse_string_replacement_op()
        }
    }

    /// Parse a string replacement op.
    ///
    /// ```ebnf
    /// <string-replacement-op> ::= "lower" | "nfd" | "nfc" | "nfkd" | "nfkc" | "remove_punct" | <trie> | <subst>
    /// <subst>                 ::= "s" <delim> <chars> <delim> <chars> <delim>
    /// ```
    fn parse_string_replacement_op(&mut self) -> Result<StringReplacementOp> {
        if self.text.starts_with(b"s") { return self.parse_regex_replacement(); }
        Ok(match self.text.take_until_any(b" \t\r\n{(") {
            b"lower" => StringReplacementOp::Lower,
            b"nfc" => StringReplacementOp::Normalise(NormalisationForm::NFC),
            b"nfd" => StringReplacementOp::Normalise(NormalisationForm::NFD),
            b"nfkc" => StringReplacementOp::Normalise(NormalisationForm::NFKC),
            b"nfkd" => StringReplacementOp::Normalise(NormalisationForm::NFKD),
            b"remove_punct" => StringReplacementOp::RemovePunct,
            b"trie" => self.parse_replacement_trie()?,
            op => {
                self.err(&format!("Unrecognised string op: '{}'", make_str(op)))?;
                unreachable!();
            }
        })
    }

    /// Parse a regex replacement op.
    fn parse_regex_replacement(&mut self) -> Result<StringReplacementOp> {
        assert!(self.text.consume(b"s")); // Yeet 's'.
        let delim = self.parse_re_delim()?;
        let regex = self.text.take_until_and_drop(&[delim]);
        let replacement = self.text.take_until_and_drop(&[delim]);
        let replacement = self.process_unicode_escapes(replacement)?;
        Ok(StringReplacementOp::Subst {
            regex: self.compile_regex(regex)?,
            replacement: SmallStr::from_str(make_str(replacement.as_ref())),
        })
    }

    /// Parse a trie for replacement.
    ///
    /// ```ebnf
    /// <trie>     ::= "trie" [ <norm> ] "{" { <pattern> } "}"
    /// <pattern>  ::= <match> "=>" <word> "\n"
    /// <norm>     ::= "(" ( "nfd" | "nfc" ) ")"
    /// <match>    ::= <class> | <word> { "|" <word> }
    /// <class>    ::= "[" { <char> | <unicode> } "]"
    /// <word>     ::= { <char> | <unicode> }+
    /// <unicode>  ::= "\x{" <hdigit> <hdigit> <hdigit> <hdigit> "}"
    /// ```
    fn parse_replacement_trie(&mut self) -> Result<StringReplacementOp> {
        // Parse normalisation form.
        let mut norm = None;
        if self.skip_ws().text.consume(b"(") {
            match self.skip_ws().text.take_until_any(b" \t\r\n)") {
                b"nfd" => norm = Some(NormalisationForm::NFD),
                b"nfc" => norm = Some(NormalisationForm::NFC),
                b"nfkc" => norm = Some(NormalisationForm::NFKC),
                b"nfkd" => norm = Some(NormalisationForm::NFKD),
                opt => self.err(&format!("Invalid trie normalisation form: {}", make_str(opt)))?,
            }
            self.skip_ws().expect(b")")?;
        }

        // Parse '{'.
        self.skip_ws().expect(b"{")?;

        // Parse <pattern>s.
        let mut patterns = Vec::<SmallStr>::new();
        let mut replacements = Vec::<SmallStr>::new();
        while !self.skip_ws().done() && !self.at(b"}") {
            let mut n = 0;

            // <class>
            //
            // We have individual bytes here, so we can't just append them to the
            // patterns; instead collect them, and then below we split the bytes into
            // chars.
            if self.text.consume(b"[") {
                let mut buffer = [0u8; 4];
                let mut bytes = SmallVec::<[u8; 64]>::new();

                // Collect individual bytes.
                while !self.done() && !self.at(b"]") {
                    if !self.at(b"\\") {
                        bytes.push(self.text.take_byte().unwrap());
                        continue;
                    }

                    // Handle escape sequences.
                    self.text.drop_byte();
                    if self.done() { self.err("Undelimited escape sequence in character class in '$ipa'")? }

                    // Handle \x{...}.
                    if self.at(b"x{") {
                        let s = Stream::new(self.text.take_until_and_drop(b"}"));
                        let seq = self.parse_unicode_escape_seq_after_backslash(s)?;
                        bytes.extend(seq);
                        continue;
                    }

                    // Handle other escape sequences.
                    let c = match self.text.take_byte().unwrap() {
                        b'r' => b'\r',
                        b't' => b'\t',
                        b'n' => b'\n',
                        b'\\' => b'\\',
                        b']' => b']',
                        c => {
                            self.err(&format!("Unrecognised escape sequence: '\\{}'", make_str(&[c])))?;
                            unreachable!();
                        }
                    };

                    bytes.push(c);
                }

                // Convert the bytes to characters and append each character.
                for c in make_str(bytes.as_ref()).chars() {
                    patterns.push(SmallStr::from_str(c.encode_utf8(&mut buffer)));
                    n += 1;
                }

                // No skip_whitespace() here as whitespace is significant within '[]'.
                self.expect(b"]")?;
            }

            // <word>
            while {
                let word = self.text.take_until_either(b"=>", b"|");
                let word = self.process_unicode_escapes(word)?;
                patterns.push(word);
                n += 1;
                self.text.consume(b"|")
            } {}

            // Parse '=>'
            self.expect(b"=>")?;

            // Get replacement text.
            self.skip_ws();
            let replacement = SmallStr::from_str(make_str(self.text.take_until_any(b" \t\r\n#")));
            let mut replacement = self.process_unicode_escapes(replacement.as_bytes())?;
            self.skip_ws();

            // Normalise it; note also that a replacement of '*' means 'delete everything.
            if replacement == "*" {
                replacement.clear();
            } else if let Some(n) = norm {
                replacement = n.apply(&replacement);
            }

            // Append 'n' copies of it to match the number of patterns we parsed; also
            // normalise the replacements if we parsed a normalisation form.
            replacements.resize(replacements.len() + n, replacement);
        }

        // Parse '}'.
        self.expect(b"}")?;

        // Normalise all patterns.
        if let Some(n) = norm {
            for p in &mut patterns { *p = n.apply(p); }
        }

        Ok(StringReplacementOp::Trie {
            replacements,
            normalisation: norm,
            trie: AhoCorasick::builder()
                .match_kind(MatchKind::LeftmostLongest)
                .build(patterns)
                .map_err(|e| make_err(
                    &self.context,
                    &format!("Failed to build trie: {}'", e.to_string())
                ))?
        })
    }

    /// Parse a unicode escape sequence.
    fn parse_unicode_escape_seq_after_backslash(&self, mut seq: Stream) -> Result<SmallVec<[u8; 4]>> {
        assert!(seq.consume(b"x{"));
        let code = seq.take_until_and_drop(b"}");
        if code.len() != 4 { self.err("Expected 4 hex digits in '\\x{...}'")? }

        // Parse the number.
        let num = u32::from_str_radix(make_str(code), 16)
            .map_err(|e| make_err(
                &self.context,
                &format!("Invalid unicode codepoint '{}': {}", make_str(code), e.to_string())
            )
        )?;

        // Convert it to a character.
        let Some(c) = char::from_u32(num) else {
            self.err(&format!("Invalid unicode codepoint '{}'", make_str(code)))?;
            unreachable!();
        };

        // Append its bytes.
        let mut buffer = [0u8; 4];
        Ok(c.encode_utf8(&mut buffer).as_bytes().into())
    }

    /// Evaluate unicode escape sequences in a string.
    fn process_unicode_escapes(&self, word: &[u8]) -> Result<SmallStr> {
        let mut word = Stream::new(word);
        if word.trim().contains_slice(b"\\x{") {
            let mut processed = SmallStr::new();
            while {
                processed.push_str(make_str(word.take_until(b"\\")));
                !word.is_empty()
            } {
                assert!(word.consume(b"\\"));
                if word.starts_with(b"x{") {
                    let s = Stream::new(word.take_until_and_drop(b"}"));
                    let seq = self.parse_unicode_escape_seq_after_backslash(s)?;
                    processed.push_str(make_str(seq.as_ref()));
                } else {
                    processed.push('\\');
                }
            }
            Ok(processed)
        } else {
            Ok(make_str(word.text()).into())
        }
    }

    /// Parse a directive that defines how fields should be preprocessed.
    ///
    /// ```ebnf
    /// <dir-preprocess> ::= "$preprocess" "{" { <rule> } "}"
    /// <rule>           ::= <field> "{" { <op> } "}"
    /// <field>          ::= "pos" | "etym" | "def" | "forms" | "ipa"
    /// <op>             ::= <match> | <string-replacement-op>
    /// <match>          ::= [ "!" ] "m" <delim> <chars> <delim> [ <message> ]
    /// <delim>          ::= "/" | "|"
    /// ```
    fn parse_preprocess_directive(&mut self) -> Result<()> {
        // Parse '{'.
        self.skip_ws().expect(b"{")?;

        // Parse rules.
        while !self.skip_ws().done() && !self.at(b"}") {
            let field = match self.text.take_until_any(b" \t\r\n{") {
                b"pos" => Part::POS,
                b"etym" => Part::Etym,
                b"def" => Part::Def,
                b"forms" => Part::Forms,
                b"ipa" => Part::IPA,
                f => return self.err(&format!("Unrecognised field: '{}'", make_str(f)))
            };

            // Parse '{'.
            self.skip_ws().expect(b"{")?;

            // Parse ops.
            while !self.skip_ws().done() && !self.at(b"}") {
                // <match>
                let op;
                if self.at(b"!m") || self.at(b"m") {
                    let negated = self.text.consume(b"!");
                    self.text.drop_byte(); // Drop 'm'.

                    // Parse regex.
                    let delim = self.parse_re_delim()?;
                    let regex = self.text.take_until_and_drop(&[delim]);
                    self.skip_ws();

                    // Parse optional message.
                    let message = if !self.text.consume(b"\"") { None }
                    else { Some(make_str(self.text.take_until_and_drop(&[b'"'])).to_string()) };
                    op = PreprocessOp::Match {
                        negated,
                        message,
                        regex: self.compile_regex(regex)?,
                    }
                } else {
                    let str_op = self.parse_string_replacement_op()?;
                    op = PreprocessOp::Replace(str_op)
                }

                self.g.preprocessor.push(PreprocessDirective { field, op })
            }

            // Parse '}'.
            self.skip_ws().expect(b"}")?;
        }

        // Parse '}'.
        self.skip_ws().expect(b"}")?;
        Ok(())
    }

    /// Parse a regular expression delimiter.
    fn parse_re_delim(&mut self) -> Result<u8> {
        // We only support a limited set of delimiters.
        if !self.text.starts_with_any(b"/|%") { self.err("Expected '|', '/', or '%' in regex")? }
        Ok(self.text.take_byte().unwrap())
    }

    /// Parse an entry.
    fn parse_entry(&mut self) -> Result<()> {
        // Get the first line. We know that it's not empty because we call
        // skip_whitespace() before we get here.
        let mut line = Cow::Borrowed(self.take_line().unwrap());

        // Add any following lines that are indented, ignoring empty lines.
        while self.text.starts_with_any(b" \t\r\n") {
            let l = self.take_line().unwrap();
            if l.is_empty() { continue }
            line.to_mut().push(b' ');
            line.to_mut().extend(l);
        }

        // Trim the input.
        let line = Stream::new(line.as_ref().trim_ascii());
        assert!(!line.is_empty());

        // If the line contains no '|' characters and a `>`,
        // it is a reference. Split by '>'. The lhs is a
        // comma-separated list of references, the rhs is the
        // actual definition.
        if !line.contains(b'|') {
            if !line.contains(b'>') { self.err("An entry must contain at least one '|' or '>'")? }
            self.disallow_specials(line.text(), "a reference entry")?;

            // Split the line into lemma and references and parse the lemma.
            let (words, target) = line.split_once(b'>').unwrap();
            let target = self.parse_tex(target)?;
            for word in Stream::new(words).split(b',') {
                let word = word.trim_ascii();
                if word.is_empty() { continue }
                let collated = self.normalise_for_sort(make_str(word));
                let word = self.parse_tex(word)?;

                // Compute search string if requested.
                let mut search = None;
                let plain_word = word.render_plain_text(true);
                if self.g.opts.populate_search_fields {
                    search = Some(normalise_for_search(&plain_word));
                }

                self.g.entries.push(Entry {
                    word,
                    plain_text_word: plain_word,
                    collated_word: collated.into_small(),
                    data: RefEntry { r#ref: target.clone(), search }
                });
            }

            return Ok(())
        }

        // Otherwise, this is a regular entry.
        const MIN_PARTS: usize = Part::Def as usize;
        const MAX_PARTS: usize = Part::Max as usize;
        let mut word: Option<&[u8]> = None;
        let mut parts = SmallVec::<[SmallCow; 5]>::new();

        // Split the line into fields.
        for part in line.split(b'|') {
            if word.is_none() {
                self.disallow_specials(part, "the lemma")?;
                word = Some(part.trim_ascii());
            } else {
                parts.push(SmallCow::Borrowed(make_str(part.trim_ascii())));
            }
        }

        // Preprocessing happens on raw strings before parsing.
        self.preprocess_full_entry(&mut parts)?;

        // Make sure we have enough parts as well as not too many parts.
        if parts.len() < MIN_PARTS {
            return self.err("An entry must have at least 4 parts: word, part of speech, etymology, definition");
        }

        if parts.len() > MAX_PARTS {
            return self.err("An entry must have at most 6 parts: word, part of speech, etymology, definition, forms, IPA");
        }

        // Part of speech.
        let pos = self.parse_tex(parts[Part::POS as usize].as_bytes())?;

        // Etymology.
        let etym_str = &parts[Part::Etym as usize];
        let mut etym = None;
        if !etym_str.as_str().is_empty() { etym = Some(self.parse_tex(etym_str.as_bytes())?) }

        // The primary definition is everything before the first sense and doesn’t
        // count as a sense because it is either the only one or, if there are multiple
        // senses, it denotes a more overarching definition that applies to all or most
        // senses.
        let mut primary_definition = None;
        let mut senses = SmallVec::<[Sense; 4]>::new();
        let mut def = Stream::new(parts[Part::Def as usize].as_bytes());
        if !def.trim().is_empty() {
            let def_text = def.take_until_and_drop(SENSE_MACRO).trim_ascii();
            if !def_text.is_empty() { primary_definition = Some(self.split_sense(def_text)?) }
            while !def.trim_start().is_empty() {
                senses.push(self.split_sense(def.take_until_and_drop(SENSE_MACRO))?);
            }
        }

        // Forms and IPA override.
        let mut forms = None;
        let mut ipa = None;
        if parts.len() > Part::Forms as usize {
            forms = Some(self.parse_tex(parts[Part::Forms as usize].as_bytes())?);
        }

        if parts.len() > Part::IPA as usize {
            ipa = Some(self.parse_tex(parts[Part::IPA as usize].as_bytes())?);
        }

        // Create a canonicalised form of this entry for sorting.
        let word = word.unwrap();
        let collated = self.normalise_for_sort(make_str(word));
        let word = self.parse_tex(word)?;
        let plain_word = word.render_plain_text(true);

        // If requested, also add search keys.
        let mut def_search = None;
        let mut hw_search = None;
        if self.g.opts.populate_search_fields {
            let mut def_search_str = match primary_definition {
                Some(ref sense) => sense.def.render_plain_text(true),
                None => SmallStr::new(),
            };

            for s in senses.iter() {
                if !def_search_str.is_empty() { def_search_str.push(' '); }
                def_search_str.push_str(&s.def.render_plain_text(true));
            }

            def_search = Some(normalise_for_search(&def_search_str));
            hw_search = Some(normalise_for_search(&plain_word));
        }

        // If requested, generate IPA.
        if ipa.is_none() && self.g.opts.always_include_ipa {
            ipa = self.to_ipa(&plain_word, true)?;
        }

        self.g.entries.push(Entry {
            word,
            plain_text_word: plain_word,
            collated_word: collated.into_small(),
            data: FullEntry {
                pos,
                etym,
                ipa,
                primary_definition,
                senses,
                forms,
                def_search,
                hw_search,
            }
        });

        Ok(())
    }

    /// Parse LaTeX.
    fn parse_tex(&self, tex: &[u8]) -> Result<Node> {
        TeXParser::parse_with_context(tex, &self.context, &self.g.custom_macros)
    }

    /// Parse LaTeX and append a full stop.
    fn parse_tex_and_add_full_stop(&self, tex: &[u8]) -> Result<Node> {
        // Recursively walk a node and append a full-stop to the end of
        // its last containing text node, if there is one, and if it doesn't
        // already end with a sentence delimiter (optionally followed by a
        // closing quotation mark).
        //
        // If we can't append a full-stop, e.g. because the last node doesn't
        // take an argument or because it is a macro whose content should not
        // be modified (e.g. \ref), this returns 'false'.
        fn add_full_stop(n: &mut Node) -> bool {
            match n {
                Node::Text(t) | Node::Math(t) => {
                    if !t.as_str().trim_end_matches(&['\'', '"', '’', '”', '»', '›']).ends_with(&['.', '?', '!']) {
                        t.push('.');
                    }

                    // Either we added a full stop or we don't need to add one.
                    true
                },
                Node::Macro { name, args } => {
                    if *name != BuiltinMacro::Reference && let Some(n) = args.last_mut() {
                        add_full_stop(n)
                    } else if *name == BuiltinMacro::Ellipsis {
                        true // Ellipsis should suppress full stop insertion.
                    } else {
                        false
                    }
                },
                Node::CustomMacro { .. } => false, // Do not attempt to recurse into custom macros.
                Node::Group(args) => {
                    if let Some(n) = args.last_mut() {
                        // Append a new node if recursing fails.
                        if !add_full_stop(n) { args.push(Node::text(".")); }
                        true
                    } else {
                        // We shouldn't really encounter an empty group, but user-defined
                        // functions might return one if a user decides to create one manually
                        // for some ungodly reason.
                        false
                    }
                },
            }
        }

        let mut node = self.parse_tex(tex)?;
        if !add_full_stop(&mut node) { node = Node::group(vec![node, Node::text(".")]) }
        Ok(node)
    }

    /// Preprocess a full entry.
    fn preprocess_full_entry(&self, parts: &mut [SmallCow]) -> Result<()> {
        for op in &self.g.preprocessor {
            let idx = op.field as usize;
            if idx >= parts.len() { continue }
            let field = &mut parts[idx];
            match &op.op {
                PreprocessOp::Match { negated, regex, message } => {
                    if regex.is_match(field.as_str()).unwrap_or(false) == *negated {
                        let m = match message {
                            Some(m) => &m,
                            None => "Precondition failed"
                        };

                        self.err(m)?;
                    }
                },

                PreprocessOp::Replace(op) => {
                    Self::apply_string_replacement_op(field, op, false);
                }
            }
        }
        Ok(())
    }

    /// Take the next line from the input and remove it. Whitespace as well as
    /// comments are stripped.
    fn take_line(&mut self) -> Option<&'s [u8]> {
        let l = self.text.take_until_and_drop(b"\n");
        let l = Stream::new(l).take_until(b"#");
        Some(l.trim_ascii())
    }

    /// Skip whitespace and comments.
    fn skip_ws(&mut self) -> &mut Self {
        loop {
            self.text.trim_start();
            if !self.text.starts_with(b"#") { break; }
            self.text.take_until_and_drop(b"\n");
        }

        self
    }

    /// Split a segment into definition, senses, and examples.
    ///
    /// If the definition contains senses, delimit each one with a dot. We
    /// do this here because it’s easier this way.
    ///
    /// A sense may contain a comment and examples; each example may also
    /// contain a comment. E.g.:
    ///
    /// \\ sense 1
    ///     \comment foo
    ///     \ex example 1
    ///          \comment comment for example 1
    ///     \ex example 2
    ///          \comment comment for example 2
    ///
    fn split_sense(&self, sense: &[u8]) -> Result<Sense> {
        let mut s = Stream::new(sense);

        // Find the sense comment or first example, if any, and depending on which comes first.
        let def_str = s.take_until_either(EX_MACRO, COMMENT_MACRO);
        let def = self.parse_tex_and_add_full_stop(def_str)?;

        // If we have a comment, parse it.
        let mut comment = None;
        if s.consume(COMMENT_MACRO) {
            if def_str.is_empty() {
                self.err("\\comment is not allowed in an empty sense or empty primary definition. Use \\i{...} instead.")?
            }

            let comment_str = s.take_until(EX_MACRO).trim_ascii();
            self.disallow_specials(comment_str, "a comment")?;
            comment = Some(self.parse_tex_and_add_full_stop(comment_str)?);
        }

        // Parse the examples.
        let mut examples = Vec::new();
        while s.trim_start().consume(EX_MACRO) {
            if def_str.is_empty() {
                self.err("\\ex is not allowed in an empty sense or empty primary definition.")?
            }

            let text = s.trim_start().take_until_either(EX_MACRO, COMMENT_MACRO);
            let mut ex_comment = None;
            if s.consume(COMMENT_MACRO) {
                let comment_str = s.take_until(EX_MACRO).trim_ascii();
                self.disallow_specials(comment_str, "a comment")?;
                ex_comment = Some(self.parse_tex_and_add_full_stop(comment_str)?);
            }

            examples.push(Example {
                text: self.parse_tex_and_add_full_stop(text)?,
                comment: ex_comment
            });
        }

        // More comments here are invalid.
        if s.trim_start().starts_with(COMMENT_MACRO) {
            self.err("Unexpected \\comment token")?;
        }

        Ok(Sense { def, comment, examples })
    }

    fn to_ipa(&self, word: &str, is_lemma: bool) -> Result<Option<Node>> {
        let Some(word) = self.g.to_unparsed_ipa_string(word, is_lemma) else { return Ok(None) };
        self.parse_tex(word.as_str().as_bytes()).map(Some)
    }
}

impl Generator {
    fn collate(&self, a: &Entry, b: &Entry) -> Ordering  {
        // If we have a collation, use it.
        if let Some(coll) = &self.collate && let Some(by) = &coll.by {
            for (c1, c2) in a.collated_word.chars().zip(b.collated_word.chars()) {
                // We remove any characters not in 'by', so the find here should
                // never fail.
                let f1 = by.iter().position(|&c| c == c1).unwrap();
                let f2 = by.iter().position(|&c| c == c2).unwrap();
                if f1 != f2 { return f1.cmp(&f2) }
            }

            // Both collated words have a common prefix; put the shorter one first. If both
            // simplified strings have the same length, also compare the original strings.
            let cmp = a.collated_word.len().cmp(&b.collated_word.len());
            if cmp != Ordering::Equal { cmp }
            else { a.plain_text_word.len().cmp(&b.plain_text_word.len()) }
        }

        // Otherwise, just do regular lexicographical string comparison.
        else {
            let cmp = a.collated_word.cmp(&b.collated_word);
            if cmp != Ordering::Equal { cmp }
            else { a.plain_text_word.cmp(&b.plain_text_word) }
        }
    }

    pub fn new(opts: Options) -> Self {
        Generator { opts, ..Self::default() }
    }

    pub fn json(&self) -> String {
        #[derive(Serialize)]
        struct Json<'a> {
            entries: &'a [Entry],
        }

        let j = Json { entries: &self.entries };
        if !self.opts.pretty_json { serde_json::to_string(&j).unwrap() }
        else {
            let mut buf = Vec::new();
            let fmt = serde_json::ser::PrettyFormatter::with_indent(b"    ");
            let mut ser = serde_json::Serializer::with_formatter(&mut buf, fmt);
            j.serialize(&mut ser).unwrap();
            String::from_utf8(buf).unwrap()
        }
    }

    pub fn parse(&mut self, input: &str) -> Result<()> {
        let mut p = Parser::new(self, input);
        p.parse()?;
        self.sort_entries();
        Ok(())
    }

    fn sort_entries(&mut self) {
        // Hack because 'collate()' borrows self: move out of the
        // member, sort, and then move back in.
        let mut moved = Vec::new();
        std::mem::swap(&mut self.entries, &mut moved);
        moved.sort_by(|a, b| self.collate(a, b));
        self.entries = moved;
    }

    #[allow(unused)] // Function used by the WASM bindings.
    pub fn to_ipa(&self, word: &str) -> Result<Option<Node>> {
        let Some(word) = self.to_unparsed_ipa_string(word, false) else { return Ok(None) };
        TeXParser::parse(word.as_bytes(), &self.custom_macros).map(Some)
    }

    fn to_unparsed_ipa_string(&self, word: &str, is_lemma: bool) -> Option<SmallStr> {
        if self.ipa_converter.is_empty() { return None }
        let mut word = SmallCow::Borrowed(word);
        for op in &self.ipa_converter { Parser::apply_string_replacement_op(&mut word, op, is_lemma); }
        Some(word.into_small())
    }
}

include!("../test/test.rs");
