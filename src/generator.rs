use crate::{string_utils::*, BuiltinMacro, Node, Nodes, Options, Part, Result};
use aho_corasick::{AhoCorasick, MatchKind};
use ariadne::{Color, Config, IndexType, Label, Report, ReportKind, Source};
use fancy_regex::Regex;
use serde::Serialize;
use smallvec::SmallVec;
use std::borrow::Cow;
use std::cmp::Ordering;
use std::collections::HashSet;
use unicode_categories::UnicodeCategories;
use unicode_normalization::UnicodeNormalization;

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

use crate::generator::StringReplacementOp::{Lower, RemovePunct};
use EntryData::*;

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

struct Parser<'text, 'map> {
    /// The file in which we're parsing; used for diagnostics.
    file: &'text InputFile<'text>,

    /// The text that we still need to parse.
    text: Stream<'text, 'map>,

    /// Generator we’re parsing into.
    g: &'text mut Generator,
}

struct TeXParser<'a> {
    file: &'a InputFile<'a>,
    custom_macros: &'a [CustomMacroDecl],
    use_colour: bool,
}

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
    loc: SourceRange,
}

#[derive(Debug)]
struct CollateDirective {
    by: Option<SmallVec<[char; 32]>>,
    ops: Vec<StringReplacementOp>,
    loc: SourceRange,
}

impl CollateDirective {
    fn make_default() -> Self {
        Self {
            by: None,
            loc: SourceRange(None),
            ops: vec![
                StringReplacementOp::Normalise(NormalisationForm::NFKD),
                RemovePunct,
                StringReplacementOp::Normalise(NormalisationForm::NFC),
                Lower,
            ]
        }
    }
}

const SENSE_MACRO: &[u8] = b"\\\\";
const EX_MACRO: &[u8] = b"\\ex";
const COMMENT_MACRO: &[u8] = b"\\comment";
const VALID_MACRO_CHARS: &[u8] = b"abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ@";

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

fn make_err(
    f: &InputFile,
    loc: &SourceRange,
    colour: bool,
    labels: &[(&str, &SourceRange, Color)]
) -> String {
    let mut message = Vec::new();
    let loc = loc.0.clone().unwrap_or(f.contents.len()..f.contents.len());
    let mut rep = Report::build(ReportKind::Error, (f.name, loc.clone()))
        .with_message("Parse Error")
        .with_config(Config::new()
            .with_color(colour)
            .with_index_type(IndexType::Byte));

    // Collect labels.
    let labels = labels.iter().map(|(msg, loc, colour)| (
        *msg,
        loc.0.clone().unwrap_or(f.contents.len()..f.contents.len()),
        *colour
    )).collect::<SmallVec<[(&str, RangeData, Color); 2]>>();

    // Sort them by location.
    for (msg, loc, colour) in labels {
        rep.add_label(Label::new((f.name, loc))
            .with_message(msg)
            .with_color(colour)
        );
    }

    let rep = rep.finish();
    if colour {
        rep.write_for_stdout((f.name, Source::from(f.contents)), &mut message).unwrap();
    } else {
        rep.write((f.name, Source::from(f.contents)), &mut message).unwrap();
    }

    // For some ungodly reason, the library sometimes writes trailing whitespace; fix that.
    let mut joined = str::from_utf8(&message).unwrap().split('\n')
        .map(|l| l.trim_end())
        .collect::<SmallVec<[&str; 20]>>()
        .join("\n");
    joined.push('\n');
    joined
}

/// Parse a unicode escape sequence.
fn parse_unicode_escape_seq(
    f: &InputFile,
    colour: bool,
    start: &SourceRange,
    s: &mut Stream
) -> Result<SmallStr> {
    macro_rules! e {
        ($loc:expr, $msg:expr) => {
            Err(make_err(f, &$loc.clone(), colour, &[($msg, &$loc.clone(), Color::Red)]))
        };
    }

    if !s.consume(b"{") { return e!(start, "Expected '{' after '\\x'"); }
    let code = s.take_until(b"}");
    if !s.consume(b"}") {
        return e!(start.extend(&s.offs()), "Missing '}' in Unicode escape sequence");
    }

    if code.len() == 0 {
        return e!(code.span(), "Expected at least 1 digit in '\\x{...}'")
    }

    // Parse the number.
    let Ok(num) = u32::from_str_radix(code.str(), 16) else {
        return e!(code.span(), &format!("Invalid unicode codepoint '{}'", code.str()));
    };

    // Convert it to a character.
    let Some(c) = char::from_u32(num) else {
        return e!(code.span(), &format!("Invalid unicode codepoint '{}'", code.str()));
    };

    // Append its bytes.
    let mut buffer = [0u8; 4];
    Ok(SmallStr::from_str(c.encode_utf8(&mut buffer)))
}

fn handle_error(errors: &mut String, res: Result<()>, keep_parsing: bool) -> Result<bool> {
    if let Err(e) = &res {
        if !keep_parsing { return Err(res.unwrap_err()) }
        if !errors.is_empty() { errors.push('\n') };
        errors.push_str(&e);
        Ok(false)
    } else {
        Ok(true)
    }
}

macro_rules! mkerr {
    ($self:expr, $loc:expr, $fmt:literal $(, $args:expr)* $(,)?) => {
        make_err(
            &$self.file,
            &$loc,
            $self.colour(),
            &[(
                &format!($fmt $(, $args)*),
                &$loc,
                Color::Red)
            ]
        )
    };
}

macro_rules! err {
    ($self:expr, $loc:expr, $fmt:literal $(, $args:expr)* $(,)?) => {
        Err(make_err(
            &$self.file,
            &$loc,
            $self.colour(),
            &[(
                &format!($fmt $(, $args)*),
                &$loc,
                Color::Red)
            ]
        ))
    };
}

impl<'a> TeXParser<'a> {
    fn colour(&self) -> bool { self.use_colour }

    /// Handle an unknown macro.
    fn handle_unknown_macro(
        &self,
        backslash: &SourceRange,
        macro_name: &Stream,
        args: Nodes,
    ) -> Result<Node> {
        for m in self.custom_macros {
            if m.name.as_bytes() == macro_name.text() {
                if m.args != args.len() as u32 {
                    return err!(
                        self,
                        backslash.extend(&macro_name.span()),
                        "Macro '\\{}' expects {} argument{}, but got {}",
                        m.name,
                        m.args,
                        if m.args == 1 { "" } else { "s" },
                        args.len()
                    );
                }

                // Ok, argument count matches.
                return Ok(Node::CustomMacro { name: m.name.clone(), args })
            }
        }

        let name = macro_name.str();
        err!(
            self,
            backslash.extend(&macro_name.span()),
            "Unknown macro '\\{}'; did you forget to '$declare {} {}' somewhere?",
            name,
            name,
            args.len()
        )
    }

    fn parse_stream(
        tex: Stream,
        file: &InputFile,
        use_colour: bool,
        custom_macros: &[CustomMacroDecl]
    ) -> Result<Node> {
        let p = TeXParser { file, custom_macros, use_colour };
        p.parse_tex(tex)
    }

    pub fn parse(s: &str, custom_macros: &[CustomMacroDecl], use_colour: bool) -> Result<Node> {
        let file = InputFile { contents: s, name: "<input>" };
        let p = TeXParser { file: &file, custom_macros, use_colour };
        let map = SourceMap::for_file(&file);
        let s = Stream::new(file.contents.as_bytes(), &map);
        p.parse_tex(s)
    }

    fn parse_tex(&self, mut s: Stream) -> Result<Node> {
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

            let text = s.take_until_any(b"\\${} \t\r\n");
            push_node(nodes, Node::text(text.str()));

            // Process the special character.
            let loc = s.offs();
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
                    if braces < 0 { return err!(self, loc, "Too many '}}'s!"); }
                }
                b' ' | b'\t' | b'\r' | b'\n' => {
                    s.trim_start();
                    push_node(nodes, Node::text(" "));
                },
                _ => break,
            }
        }

        // If 'braces' is initially 0, it’s possible for us to get here without
        // ever encountering a closing brace. This happens frequently if this
        // function is invoked at the top-level of the parser.
        if braces != 0 { err!(self, s.offs(), "Unexpected end of input. Did you forget a '}}'?") }
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
        let backslash = s.offs();
        assert!(s.consume(b"\\"));
        if s.is_empty() { err!(self, backslash, "Invalid macro escape sequence")? }

        // Soft hyphen.
        if s.consume(b"-") { return Ok(Node::builtin(BuiltinMacro::SoftHyphen)) }

        // '\\' is invalid here.
        if s.starts_with(b"\\") {
            err!(self, backslash.extend(&s.offs()), "'\\\\' cannot be used in this field")?
        }

        // These are treated literally.
        if s.starts_with_any(b" &$%#{}") {
            return Ok(Node::text(s.take(1).str()))
        }

        // These are unsupported single-character macros.
        if s.starts_with_any(b"!/:@[]`{~") {
            return self.handle_unknown_macro(&backslash, &s.take(1), vec![]);
        }

        // Handle builtin macros.
        let macro_name = s.take_while_any(VALID_MACRO_CHARS);
        match macro_name.text() {
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
            b"x" => {
                let c = parse_unicode_escape_seq(self.file, self.colour(), &macro_name.span(), s)?;
                Ok(Node::text(c))
            },
            b"ex" | b"comment" => {
                err!(self, backslash.extend(&macro_name.span()), "'\\{}' cannot be used in this field", macro_name)
            },
            b"label" => {
                let _ = self.parse_tex_group(s)?; // Throw away the argument.
                Ok(Node::empty())
            }
            _ => {
                let mut args = Nodes::new();
                while s.starts_with(b"{") { args.push(self.parse_tex_group(s)?); }
                self.handle_unknown_macro(&backslash, &macro_name, args)
            }
        }
    }

    fn parse_tex_maths(&self, s: &mut Stream) -> Result<Node> {
        let loc = s.offs();
        assert!(s.consume(b"$"));
        let node = Node::Math(s.take_until(b"$").str().into()); // TODO: Support maths properly.
        if !s.consume(b"$") { err!(self, loc, "Could not find a closing '$'; if you meant to write a literal dollar sign, write '\\$'") }
        else { Ok(node) }
    }

    fn parse_tex_single_arg_builtin_macro(&self, s: &mut Stream, m: BuiltinMacro) -> Result<Node> {
        // Drop everything until the argument brace. We’re not a LaTeX tokeniser, so we don’t
        // support stuff like `\fract1 2`, as much as I like to write it.
        if !s.trim_start().starts_with(b"{") {
            return err!(self, s.offs(), "Sorry, macro arguments must be enclosed in braces");
        }

        let arg = self.parse_tex_group(s)?;
        Ok(Node::builtin_with_args(m, vec![arg]))
    }
}

impl<'text, 'map> Parser<'text, 'map> {
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
    fn at(&self, what: &[u8]) -> bool { self.text.starts_with(what) }

    /// Whether we should use diagnostics colours.
    fn colour(&self) -> bool { self.g.opts.colour }

    /// Helper to compile a user-provided regex.
    fn compile_regex(&self, re: Stream) -> Result<Regex> {
        let regex = re.str();
        Regex::new(regex).map_err(|e| mkerr!(
            self,
            re.span(),
            "Failed to compile regex '{}': {}",
            regex,
            e.to_string()
        ))
    }

    /// Consume a delimiter.
    fn consume_delim(&mut self, delim: u8, delim_loc: &SourceRange) -> Result<()> {
        if !self.text.consume(&[delim]) {
            err!(self, delim_loc, "Unmatched '{}' delimiter", delim as char)
        } else {
            Ok(())
        }
    }

    /// '\\ex' and friends are control macros that are really part of the dictionary
    /// syntax rather than markup; this function is used to disallow them in certain
    /// contexts.
    fn disallow_specials(&self, s: &Stream, context: &str) -> Result<()> {
        for special in [COMMENT_MACRO, EX_MACRO, SENSE_MACRO] {
            if let Some(idx) = s.find(special) {
                return err!(
                    self,
                    s.clone().drop(idx).take(special.len()).span(),
                    "'{}' cannot be used in {}",
                    make_str(special),
                    context
                );
            }
        }

        Ok(())
    }

    /// Check if we're done parsing.
    fn done(&self) -> bool { self.text.is_empty() }

    /// Expect a string.
    fn expect(&mut self, what: &[u8]) -> Result<()> {
        if self.text.consume(what) { Ok(()) }
        else {
            err!(
                self,
                self.text.offs(),
                "Expected '{}', but found '\\x{:x}'",
                make_str(what),
                self.text.front().unwrap_or(0)
            )
        }
    }

    /// Normalise a string for sorting.
    fn normalise_for_sort<'a>(&self, text: &Stream<'a, '_>) -> Result<SmallCow<'a>> {
        match &self.g.collate {
            None => panic!("Collation not initialised!"),
            Some(coll) => {
                let mut str = SmallCow::Borrowed(text.str());

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

                // If the collation creates an empty word, we have a problem.
                if str.as_str().trim_ascii().is_empty() {
                    err!(self, text.span(), "Collation of '{}' resulted in an empty word", text)?;
                }

                Ok(str)
            },
        }
    }

    /// Parse the input.
    fn parse(&mut self) -> Result<()> {
        let mut errors = String::new();

        // Parse directives.
        //
        // Directives modify how entries are processed, so if a directive is defined after
        // we've already seen some entries, it won't be applied to those; this is almost
        // certainly not what you want.
        while !self.skip_ws().done() && self.at(b"$") {
            if !handle_error(
                &mut errors,
                self.parse_directive(),
                self.g.opts.keep_parsing
            )? {
                self.text.take_until(b"\n");
                self.skip_ws();
            }
        }

        // Get the default collation if the user didn't define one.
        if self.g.collate.is_none() { self.g.collate = Some(CollateDirective::make_default()); }

        // Parse entries.
        while !self.skip_ws().done() {
            if self.at(b"$") { return err!(self, self.text.offs(), "Directives must precede all entries"); }
            if !handle_error(
                &mut errors,
                self.parse_entry(),
                self.g.opts.keep_parsing
            )? {
                self.text.take_until(b"\n");
                self.skip_ws();
            }
        }

        if !errors.is_empty() { Err(errors) }
        else { Ok(()) }
    }

    /// Parse a directive.
    ///
    /// ```ebnf
    /// <directive> ::= <dir-ipa> | <dir-preprocess> | <dir-declare> | <dir-collate>
    /// ```
    fn parse_directive(&mut self) -> Result<()> {
        let start = self.text.offs();
        assert!(self.text.consume(b"$"));
        let dir = self.text.take_until_any(b" \t\r\n{");
        match dir.text() {
            b"collate" => self.parse_collate_directive(start.extend(&dir.span())),
            b"declare" => self.parse_declare_directive(),
            b"ipa" => self.parse_ipa_directive(),
            b"preprocess" => self.parse_preprocess_directive(),
            _ => err!(self, start.extend(&dir.span()), "Unrecognised directive: '{}'", dir.str())
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
    fn parse_collate_directive(&mut self, loc: SourceRange) -> Result<()> {
        // Parse '{'.
        self.skip_ws().expect(b"{")?;

        // Parse clauses.
        let mut by = None;
        let mut ops = Vec::new();
        while !self.skip_ws().done() && !self.at(b"}") {
            let clause = self.text.take_until_any(b" \t\r\n{");
            match clause.text() {
                b"by" => {
                    if by.is_some() { return err!(self, clause.span(), "Duplicate 'by' in $collate directive"); }
                    self.skip_ws();
                    by = Some(self.parse_string()?.chars().collect());
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
                _ => {
                    return err!(self, clause.span(), "Unknown clause in $collate: {}", clause.str());
                }
            }
        }

        // Parse '}'.
        self.skip_ws().expect(b"}")?;

        // Two collation directives don't really make sense.
        if let Some(c) = &self.g.collate {
            return Err(make_err(
                &self.file,
                &loc,
                self.colour(),
                &[
                    ("Previous directive was here", &c.loc, Color::Cyan),
                    ("A '$collate' directive was already defined earlier", &loc, Color::Red)
                ]
            ));
        }

        self.g.collate = Some(CollateDirective { by, ops, loc });
        Ok(())
    }


    /// Parse a macro declaration directive.
    ///
    /// ```ebnf
    /// <dir-declare> ::= "$declare" <name> <number> "\n"
    /// ```
    fn parse_declare_directive(&mut self) -> Result<()> {
        let mut name = self.skip_ws().text.take_until_any(b" \t");
        if let Some(idx) = name.find_first_not_of(VALID_MACRO_CHARS) {
            name.drop(idx);
            let len = name.measure_first_char().unwrap();
            let c = name.take(len);
            return err!(
                self,
                c.span(),
                "Invalid character '{}' in macro name",
                c.str()
            )
        }

        let args = self.skip_ws().text.take_until_any(b" \t\r\n");
        let args = args.str().parse::<u32>().map_err(|e|
            mkerr!(
                self,
                args.span(),
                "Argument count '{}' is not a valid integer: {}",
                args,
                e.to_string(),
            )
        )?;

        let mut junk = self.text.take_until_any(b"\n#");
        junk.trim_start();
        if !junk.is_empty() {
            return err!(self, junk.span(), "Extraneous junk at the end of $declare directive");
        }

        self.skip_ws();
        for m in &self.g.custom_macros {
            if m.name.as_bytes() == name.text() {
                let loc = name.span();
                return Err(make_err(
                    &self.file,
                    &loc,
                    self.colour(),
                    &[
                        ("Previously declared here", &m.loc, Color::Cyan),
                        (&format!("Custom macro '{}' was already declared", name), &loc, Color::Red),
                    ]
                ));
            }
        }

        self.g.custom_macros.push(CustomMacroDecl {
            name: name.str().into(),
            args,
            loc: name.span()
        });

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
    /// <lemma-op>   ::= "lemma" "{" { <string-replacement-op> } "}"
    /// ```
    fn parse_ipa_replacement_op(&mut self) -> Result<StringReplacementOp> {
        if self.text.consume(b"lemma") {
            // Parse '{'.
            self.skip_ws().expect(b"{")?;

            // Parse ops recursively.
            let mut ops = Vec::new();
            while !self.skip_ws().done() && !self.at(b"}") {
                let loc = self.text.offs();
                let op = self.parse_ipa_replacement_op()?;
                if matches!(op, StringReplacementOp::Lemma { .. }) {
                    return err!(self, loc, "'lemma {{}}' cannot appear within 'lemma {{}}'");
                }
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
        let op = self.text.take_until_any(b" \t\r\n{(");
        Ok(match op.text() {
            b"lower" => StringReplacementOp::Lower,
            b"nfc" => StringReplacementOp::Normalise(NormalisationForm::NFC),
            b"nfd" => StringReplacementOp::Normalise(NormalisationForm::NFD),
            b"nfkc" => StringReplacementOp::Normalise(NormalisationForm::NFKC),
            b"nfkd" => StringReplacementOp::Normalise(NormalisationForm::NFKD),
            b"remove_punct" => StringReplacementOp::RemovePunct,
            b"trie" => self.parse_replacement_trie(&op.span())?,
            _ => return err!(self, op.span(), "Unrecognised string op: '{}'", op.str())
        })
    }

    /// Parse a regex replacement op.
    fn parse_regex_replacement(&mut self) -> Result<StringReplacementOp> {
        assert!(self.text.consume(b"s")); // Yeet 's'.

        // Parse delimiter.
        let (delim, delim_loc) = self.parse_re_delim()?;

        // Parse regex.
        let regex = self.text.take_until_any(&[delim, b'\n']);
        let new_delim_loc = self.text.offs();
        self.consume_delim(delim, &delim_loc)?;

        // Parse replacement.
        let replacement = self.text.take_until_any(&[delim, b'\n']);
        self.consume_delim(delim, &new_delim_loc)?;

        // Create the op.
        let replacement = self.process_unicode_escapes(replacement)?;
        Ok(StringReplacementOp::Subst {
            regex: self.compile_regex(regex)?,
            replacement: replacement.into_small(),
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
    fn parse_replacement_trie(&mut self, loc: &SourceRange) -> Result<StringReplacementOp> {
        // Parse normalisation form.
        let mut norm = None;
        if self.skip_ws().text.consume(b"(") {
            let op = self.skip_ws().text.take_until_any(b" \t\r\n)");
            match op.text() {
                b"nfd" => norm = Some(NormalisationForm::NFD),
                b"nfc" => norm = Some(NormalisationForm::NFC),
                b"nfkc" => norm = Some(NormalisationForm::NFKC),
                b"nfkd" => norm = Some(NormalisationForm::NFKD),
                _ => err!(self, op.span(), "Invalid trie normalisation form: {}", op.str())?,
            }
            self.skip_ws().expect(b")")?;
        }

        // Parse '{'.
        self.skip_ws().expect(b"{")?;

        // Parse <pattern>s.
        let mut patterns = Vec::<(SmallStr, SourceRange)>::new();
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
                let pattern_loc = self.text.clone().take_until(b"=>").span();

                // Collect individual bytes.
                while !self.done() && !self.at(b"]") {
                    if !self.at(b"\\") {
                        bytes.push(self.text.take_byte().unwrap());
                        continue;
                    }

                    // Handle escape sequences.
                    let backslash = self.text.offs();
                    self.text.drop_byte();
                    if self.done() {
                        return err!(self, backslash, "Undelimited escape sequence in character class in '$ipa'")
                    }

                    // Handle \x{...}.
                    if self.at(b"x{") {
                        self.text.drop_byte();
                        let seq = parse_unicode_escape_seq(self.file, self.colour(), &backslash, &mut self.text)?;
                        bytes.extend_from_slice(seq.as_bytes());
                        continue;
                    }

                    // Handle other escape sequences.
                    let c = self.text.take(1);
                    let c = match c.text()[0] {
                        b'r' => b'\r',
                        b't' => b'\t',
                        b'n' => b'\n',
                        b'\\' => b'\\',
                        b']' => b']',
                        _ => return err!(self, c.span(), "Unrecognised escape sequence: '\\{}'", c.str())
                    };

                    bytes.push(c);
                }

                // Convert the bytes to characters and append each character.
                let mut offs = 0;
                for c in make_str(bytes.as_ref()).chars() {
                    let s = c.encode_utf8(&mut buffer);
                    patterns.push((
                        SmallStr::from_str(s),
                        pattern_loc.offset(offs).take(s.len())
                    ));

                    offs += s.len();
                    n += 1;
                }

                // No skip_whitespace() here as whitespace is significant within '[]'.
                self.expect(b"]")?;
            }

            // <word>
            else {
                while {
                    let mut word = self.text.take_until_either(b"=>", b"|");
                    let loc = word.trim().span();
                    let word = self.process_unicode_escapes(word)?;
                    patterns.push((word.into_small(), loc));
                    n += 1;
                    self.text.consume(b"|")
                } {}
            }

            // Parse '=>'
            self.skip_ws().expect(b"=>")?;

            // Get replacement text.
            self.skip_ws();
            let replacement = self.text.take_until_any(b" \t\r\n#");
            let mut replacement = self.process_unicode_escapes(replacement)?;
            self.skip_ws();

            // Normalise it; note also that a replacement of '*' means 'delete everything.
            if replacement.as_str() == "*" {
                replacement.clear();
            } else if let Some(n) = norm {
                replacement = SmallCow::Small(n.apply(replacement.as_str()));
            }

            // Append 'n' copies of it to match the number of patterns we parsed; also
            // normalise the replacements if we parsed a normalisation form.
            replacements.resize(replacements.len() + n, replacement.into_small());
        }

        // Parse '}'.
        self.expect(b"}")?;

        // Normalise all patterns.
        if let Some(n) = norm {
            for (p, _) in &mut patterns { *p = n.apply(p); }
        }

        // The patterns must not contain duplicates.
        let mut unique = HashSet::new();
        for (p, loc) in &patterns {
            if !unique.insert(p.as_str()) {
                let (_, prev) = patterns.iter().find(|(pat, _)| pat == p).unwrap();
                return Err(make_err(
                    &self.file,
                    &loc,
                    self.colour(),
                    &[
                        ("Previous instance was here", &prev, Color::Cyan),
                        (&format!("Duplicate pattern '{}' in replacement trie", p), &loc, Color::Red),
                    ]
                ));
            }
        }

        assert_eq!(patterns.len(), replacements.len());
        Ok(StringReplacementOp::Trie {
            replacements,
            normalisation: norm,
            trie: AhoCorasick::builder()
                .match_kind(MatchKind::LeftmostLongest)
                .build(patterns.iter().map(|(p, _)|p))
                .map_err(|e| mkerr!(
                    self,
                    loc,
                    "Failed to build trie: {}'",
                    e.to_string()
                ))?
        })
    }

    /// Evaluate unicode escape sequences in a string.
    fn process_unicode_escapes<'a>(&self, mut word: Stream<'a, '_>) -> Result<SmallCow<'a>> {
        if word.trim().contains_slice(b"\\x{") {
            let mut processed = SmallStr::new();
            while {
                processed.push_str(word.take_until(b"\\").str());
                !word.is_empty()
            } {
                let loc = word.offs();
                assert!(word.consume(b"\\"));
                if word.starts_with(b"x{") {
                    word.drop_byte();
                    let seq = parse_unicode_escape_seq(self.file, self.colour(), &loc, &mut word)?;
                    processed.push_str(&seq);
                } else {
                    processed.push('\\');
                }
            }
            Ok(SmallCow::Small(processed))
        } else {
            Ok(SmallCow::Borrowed(word.str()))
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
            let field = self.text.take_until_any(b" \t\r\n{");
            let field = match field.text() {
                b"pos" => Part::POS,
                b"etym" => Part::Etym,
                b"def" => Part::Def,
                b"forms" => Part::Forms,
                b"ipa" => Part::IPA,
                _ => return err!(self, field.span(), "Unrecognised field: '{}'", field.str())
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
                    let (delim, delim_loc) = self.parse_re_delim()?;
                    let regex = self.text.take_until_any(&[delim, b'\n']);
                    self.consume_delim(delim, &delim_loc)?;
                    self.skip_ws();

                    // Parse optional message.
                    let message = if self.text.starts_with(b"\"") {
                        Some(self.parse_string()?.to_string())
                    } else {
                        None
                    };

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

    /// Parse a literal string.
    fn parse_string(&mut self) -> Result<&str> {
        let loc = self.text.offs();
        self.expect(b"\"")?;
        let s = self.text.take_until_any(&[b'\"', b'\n']).str();
        self.consume_delim(b'"', &loc)?;
        Ok(s)
    }

    /// Parse a regular expression delimiter.
    fn parse_re_delim(&mut self) -> Result<(u8, SourceRange)> {
        // We only support a limited set of delimiters.
        if !self.text.starts_with_any(b"/|%") {
            err!(self, self.text.offs(), "Expected '|', '/', or '%' in regex")
        } else {
            let d = self.text.take(1);
            Ok((d.text()[0], d.span()))
        }
    }

    /// Parse an entry.
    fn parse_entry(&mut self) -> Result<()> {
        let mut ranges = SmallVec::<[(RangeData, SourceRange); 5]>::new();

        // Get the first line. We know that it's not empty because we call
        // skip_whitespace() before we get here.
        let first = self.take_line().unwrap();
        let mut map = SourceMap::new();
        ranges.push((0..first.len(), first.span()));

        // Add any following lines that are indented, ignoring empty lines.
        //
        // Avoiding the copies doesn't work because of line comments: even if
        // we skip line comments in the TeX parser afterwards, we end up with
        // trailing whitespace because of that (because trimming doesn't trim
        // comments); while fixing that *might* be possible, it's honestly not
        // worth it at that point...
        let mut line: Cow<'text, [u8]> = Cow::Borrowed(first.text());
        while self.text.starts_with_any(b" \t\r\n#") {
            let l = self.take_line().unwrap();
            if l.is_empty() { continue }

            // Add a space and place it at a fake location.
            ranges.push((line.len()..(line.len() + 1), SourceRange(None)));
            line.to_mut().push(b' ');

            // Add the line.
            ranges.push((line.len()..(line.len() + l.text().len()), l.span()));
            line.to_mut().extend(l.text());
        }

        // Now that the line has been finalised, build the source map. we only need
        // to do this if the line's data isn't from the original file anymore.
        if matches!(line, Cow::Owned(_)) {
            for (virt, physical) in ranges {
                map.add(&line[virt], physical);
            }
        }

        // Now parse the entry.
        let mut line = Stream::new(line.as_ref(), &map);
        assert!(!line.is_empty());

        // If the line contains no '|' characters and a `>`,
        // it is a reference. Split by '>'. The lhs is a
        // comma-separated list of references, the rhs is the
        // actual definition.
        if !line.contains(b'|') {
            if !line.contains(b'>') {
                return err!(
                    self,
                    line.take_until(b"\n").span(),
                    "An entry must contain at least one '|' or '>'"
                );
            }

            self.disallow_specials(&line, "a reference entry")?;

            // Split the line into lemma and references and parse the lemma.
            let (words, target) = line.split_once(b'>').unwrap();
            let target = self.parse_tex(target)?;
            for mut word in words.split(b',') {
                if word.trim().is_empty() { continue }
                let collated = self.normalise_for_sort(&word)?;
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
        const MIN_PARTS: usize = Part::Def as usize + 1;
        const MAX_PARTS: usize = Part::Max as usize;
        let mut word: Option<&[u8]> = None;
        let mut parts = SmallVec::<[SmallCow; 5]>::new();

        // Split the line into fields.
        for mut part in line.split(b'|') {
            part.trim();
            if word.is_none() {
                self.disallow_specials(&part, "the lemma")?;
                word = Some(part.text());
            } else {
                parts.push(SmallCow::Borrowed(part.str()));
            }
        }

        // Preprocessing happens on raw strings before parsing.
        let loc = line.take_until(b"\n").span();
        self.preprocess_full_entry(&mut parts, &mut map)?;

        // Make sure we have enough parts as well as not too many parts.
        if parts.len() < MIN_PARTS {
            return err!(
                self,
                loc,
                "An entry must have at least 4 parts: word, part of speech, etymology, definition"
            );
        }

        if parts.len() > MAX_PARTS {
            return err!(
                self,
                loc,
                "An entry must have at most 6 parts: word, part of speech, etymology, definition, forms, IPA"
            );
        }

        // Part of speech.
        let pos = self.parse_tex(Stream::new(parts[Part::POS as usize].as_bytes(), &map))?;

        // Etymology.
        let etym_str = &parts[Part::Etym as usize];
        let mut etym = None;
        if !etym_str.as_str().is_empty() {
            etym = Some(self.parse_tex(Stream::new(etym_str.as_bytes(), &map))?)
        }

        // The primary definition is everything before the first sense and doesn’t
        // count as a sense because it is either the only one or, if there are multiple
        // senses, it denotes a more overarching definition that applies to all or most
        // senses.
        let mut primary_definition = None;
        let mut senses = SmallVec::<[Sense; 4]>::new();
        let mut def = Stream::new(parts[Part::Def as usize].as_bytes(), &map);
        if def.trim().is_empty() { return err!(self, def.span(), "Definition must not be empty"); }

        let mut def_text = def.take_until_and_drop(SENSE_MACRO);
        if !def_text.trim().is_empty() { primary_definition = Some(self.split_sense(def_text)?) }
        while !def.trim_start().is_empty() {
            senses.push(self.split_sense(def.take_until_and_drop(SENSE_MACRO))?);
        }

        // Forms and IPA override.
        let mut forms = None;
        let mut ipa = None;
        if parts.len() > Part::Forms as usize {
            forms = Some(self.parse_tex(Stream::new(parts[Part::Forms as usize].as_bytes(), &map))?);
        }

        if parts.len() > Part::IPA as usize {
            ipa = Some(self.parse_tex(Stream::new(parts[Part::IPA as usize].as_bytes(), &map))?);
        }

        // The word must not be empty.
        let mut word_stream = Stream::new(word.unwrap(), &map);
        if word_stream.trim().is_empty() { return err!(self, word_stream.span(), "Lemma must not be empty"); }

        // Create a canonicalised form of this entry for sorting.
        let collated = self.normalise_for_sort(&word_stream)?.into_small();
        let word = self.parse_tex(word_stream.clone())?;
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
            if let Some(word) = self.g.to_unparsed_ipa_string(plain_word.as_str(), true) {
                map.add(word.as_bytes(), word_stream.span());
                ipa = Some(self.parse_tex(Stream::new(word.as_bytes(), &map))?);
            }
        }

        self.g.entries.push(Entry {
            word,
            plain_text_word: plain_word,
            collated_word: collated,
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
    fn parse_tex(&self, tex: Stream) -> Result<Node> {
        TeXParser::parse_stream(tex, &self.file, self.colour(), &self.g.custom_macros)
    }

    /// Parse LaTeX and append a full stop.
    fn parse_tex_and_add_full_stop(&self, tex: Stream) -> Result<Node> {
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
    fn preprocess_full_entry(&self, parts: &mut [SmallCow], map: &mut SourceMap) -> Result<()> {
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

                        err!(self, Stream::new(field.as_bytes(), &map).span(), "{}", m)?;
                    }
                },

                PreprocessOp::Replace(op) => {
                    // Save the location and apply the replacement.
                    let loc = Stream::new(field.as_bytes(), &map).span();
                    Self::apply_string_replacement_op(field, op, false);

                    // If that changed something, add the location of the new string to the map.
                    if matches!(field, SmallCow::Borrowed(_)) { continue };
                    map.add(field.as_bytes(), loc);
                }
            }
        }
        Ok(())
    }

    /// Take the next line from the input and remove it. Whitespace as well as
    /// comments are stripped.
    fn take_line(&mut self) -> Option<Stream<'text, 'map>> {
        let mut l = self.text.take_until_and_drop(b"\n");
        l = l.take_until(b"#");
        l.trim();
        Some(l)
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
    fn split_sense(&self, mut s: Stream) -> Result<Sense> {
        // Find the sense comment or first example, if any, and depending on which comes first.
        let def_str = s.take_until_either(EX_MACRO, COMMENT_MACRO);
        let def = self.parse_tex_and_add_full_stop(def_str.clone())?;

        // If we have a comment, parse it.
        let mut comment = None;
        if s.consume(COMMENT_MACRO) {
            if def_str.is_empty() {
                return err!(
                    self,
                    def_str.span(),
                    "\\comment is not allowed in an empty sense or empty primary definition. Use \\i{{...}} instead."
                );
            }

            let mut comment_str = s.take_until(EX_MACRO);
            comment_str.trim();
            self.disallow_specials(&comment_str, "a comment")?;
            comment = Some(self.parse_tex_and_add_full_stop(comment_str)?);
        }

        // Parse the examples.
        let mut examples = Vec::new();
        while s.trim_start().consume(EX_MACRO) {
            if def_str.is_empty() {
                return err!(
                    self,
                    def_str.span(),
                    "\\ex is not allowed in an empty sense or empty primary definition."
                );
            }

            let text = s.trim_start().take_until_either(EX_MACRO, COMMENT_MACRO);
            let mut ex_comment = None;
            if s.consume(COMMENT_MACRO) {
                let mut comment_str = s.take_until(EX_MACRO);
                comment_str.trim();
                self.disallow_specials(&comment_str, "a comment")?;
                ex_comment = Some(self.parse_tex_and_add_full_stop(comment_str)?);
            }

            examples.push(Example {
                text: self.parse_tex_and_add_full_stop(text)?,
                comment: ex_comment
            });
        }

        // More comments here are invalid.
        if s.trim_start().starts_with(COMMENT_MACRO) {
            err!(self, s.take(COMMENT_MACRO.len()).span(), "Unexpected \\comment token")?;
        }

        Ok(Sense { def, comment, examples })
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

    pub fn parse(&mut self, input: &str, filename: &str) -> Result<()> {
        let f = InputFile { contents: input, name: filename };
        let source_map = SourceMap::for_file(&f);
        let mut p = Parser {
            text: Stream::new(input.as_bytes(), &source_map),
            file: &f,
            g: self
        };

        p.parse()?;
        p.g.sort_entries();
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
        TeXParser::parse(word.as_str(), &self.custom_macros, self.opts.colour).map(Some)
    }

    fn to_unparsed_ipa_string(&self, word: &str, is_lemma: bool) -> Option<SmallStr> {
        if self.ipa_converter.is_empty() { return None }
        let mut word = SmallCow::Borrowed(word);
        for op in &self.ipa_converter { Parser::apply_string_replacement_op(&mut word, op, is_lemma); }
        Some(word.into_small())
    }
}

include!("../test/test.rs");
