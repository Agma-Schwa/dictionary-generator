use std::{borrow::Cow};

use serde::Serialize;
use smallvec::SmallVec;
use unicode_categories::UnicodeCategories;
use unicode_normalization::UnicodeNormalization;

use crate::{Node, Nodes, BuiltinMacro, string_utils::*};

use crate::{LanguageOps, Part, Result};

const SENSE_MACRO: &str = "\\\\";
const EX_MACRO: &str = "\\ex";
const COMMENT_MACRO: &str = "\\comment";

#[derive(Serialize)]
struct Entry {
    word: Node,
    #[serde(skip)] nfkd: String,
    #[serde(skip)] word_for_sorting: String,
    #[serde(flatten)] data: EntryData,
}

#[derive(Serialize)]
#[serde(untagged)]
enum EntryData {
    RefEntry {
        refs: Vec<Node>,
        #[serde(skip_serializing_if = "Option::is_none")] search: Option<String>,
    },
    FullEntry {
        /// Part of speech.
        pos: Node,

        /// Etymology.
        etym: Node,

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

#[derive(Serialize)]
struct Sense {
    def: Node,
    #[serde(skip_serializing_if = "Option::is_none")] comment: Option<Node>,
    #[serde(skip_serializing_if = "Vec::is_empty")] examples: Vec<Example>,
}

#[derive(Serialize)]
struct Example {
    text: Node,
    #[serde(skip_serializing_if = "Option::is_none")] comment: Option<Node>,
}

pub struct Generator {
    line: u32, // The current line.
    ops: Box<dyn LanguageOps>,
    entries: Vec<Entry>,
    populate_search_fields: bool,
}

impl Generator {
    /// '\\ex' and friends are control macros that are really part of the dictionary
    /// syntax rather than markup; this function is used to disallow them in certain
    /// contexts.
    fn disallow_specials(&self, text: &str, context: &str) -> Result<()> {
        for special in [COMMENT_MACRO, EX_MACRO, SENSE_MACRO] {
            if text.contains(special) {
                return self.err(&format!("'{}' cannot be used in {}", special, context));
            }
        }

        Ok(())
    }

    fn err(&self, msg: &str) -> Result<()> {
        Err(format!("Error near line {}: {}", self.line, msg))
    }

    pub(crate) fn new(ops: Box<dyn LanguageOps>, populate_search_fields: bool) -> Self {
        Generator {
            line: 0,
            ops,
            entries: vec![],
            populate_search_fields
        }
    }

    fn normalise_for_search(&self, text: &str) -> String {
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

    fn normalise_for_sort(&self, text: &str) -> String {
        let text: String = text
            .nfkd()
            .filter(|c| !c.is_mark() && !c.is_punctuation())
            .nfc()
            .collect();

        text.to_lowercase()
    }

    pub fn json(&mut self) -> String {
        #[derive(Serialize)]
        struct Json<'a> {
            entries: &'a [Entry],
        }

        self.entries.sort_by(|a, b| self.ops.collate(
            &a.word_for_sorting,
            &b.word_for_sorting,
            &a.nfkd,
            &b.nfkd
        ));

        let j = Json { entries: &self.entries };
        serde_json::to_string(&j).unwrap()
    }

    pub fn parse(&mut self, input: &str) -> Result<()> {
        self.line = 0;
        let mut logical_line: Cow<'_, str> = Cow::Borrowed("");
        for mut l in input.lines() {
            self.line += 1;

            // Drop comments.
            match l.split_once('#') {
                None => {},
                Some((rest, _)) => l = rest,
            }

            // Skip empty lines.
            if l.is_empty() { continue }

            // Check for directives.
            if l.starts_with('$') { return self.err("Directives are no longer supported."); }

            // Perform line continuation.
            if l.starts_with(' ') || l.starts_with('\t') {
                logical_line.to_mut().push(' ');
                logical_line.to_mut().push_str(l.trim());
                continue
            }

            // This line starts a new entry, so ship out the last
            // one and start a new one.
            self.parse_entry(&logical_line)?;

            // Copying the line here may seem a little wasteful, but it turns out
            // that a lot if not most entries span multiple lines, so it
            logical_line = Cow::Borrowed(l);
        }

        // Make sure to also process the last line.
        self.parse_entry(&logical_line)
    }

    fn parse_entry(&mut self, mut line: &str) -> Result<()> {
        line = line.trim();

        // Ignore empty lines here.
        if line.is_empty() { return Ok(()) }

        // If the line contains no '|' characters and a `>`,
        // it is a reference. Split by '>'. The lhs is a
        // comma-separated list of references, the rhs is the
        // actual definition.
        if !line.contains('|') {
            if !line.contains('>') { return self.err("An entry must contain at least one '|' or '>'") }
            self.disallow_specials(line, "a reference entry")?;

            // Split the line into lemma and references and parse the lemma.
            let (word, target_str) = line.split_once('>').unwrap();
            let nfkd = self.normalise_for_sort(word);
            let word = self.parse_tex(word)?;

            // Split the references.
            let mut refs = Vec::new();
            for entry in target_str.split(',') { refs.push(self.parse_tex(entry)?); }

            // Compute search string if requested.
            let mut search = None;
            let plain_word = word.render_plain_text(true);
            if self.populate_search_fields {
                search = Some(self.normalise_for_search(&plain_word));
            }

            self.entries.push(Entry {
                word,
                word_for_sorting: plain_word,
                nfkd,
                data: RefEntry { refs, search }
            });

            return Ok(())
        }

        // Otherwise, this is a regular entry.
        const MIN_PARTS: usize = Part::Def as usize;
        const MAX_PARTS: usize = Part::Max as usize;
        let mut word: Option<&str> = None;
        let mut parts = SmallVec::<[Cow<'_, str>; 5]>::new();

        // Split the line into fields.
        for part in line.split('|') {
            if word.is_none() {
                self.disallow_specials(part, "the lemma")?;
                word = Some(part);
            } else {
                parts.push(Cow::Borrowed(part.trim()));
            }
        }

        // Preprocessing happens on raw strings before parsing.
        self.ops.preprocess_full_entry(&mut parts)?;

        // Make sure we have enough parts as well as not too many parts.
        if parts.len() < MIN_PARTS {
            return self.err("An entry must have at least 4 parts: word, part of speech, etymology, definition");
        }

        if parts.len() > MAX_PARTS {
            return self.err("An entry must have at most 6 parts: word, part of speech, etymology, definition, forms, IPA");
        }

        // Part of speech and etymology.
        let pos = self.parse_tex(&parts[Part::POS as usize])?;
        let etym = self.parse_tex(&parts[Part::Etym as usize])?;

        // The primary definition is everything before the first sense and doesn’t
        // count as a sense because it is either the only one or, if there are multiple
        // senses, it denotes a more overarching definition that applies to all or most
        // senses.
        let mut primary_definition = None;
        let mut senses = SmallVec::<[Sense; 4]>::new();
        match &parts[Part::Def as usize].split_once(SENSE_MACRO) {
            None => { primary_definition = Some(self.split_sense(&parts[Part::Def as usize])?) },
            Some((def, rest)) => {
                if !def.trim().is_empty() { primary_definition = Some(self.split_sense(def)?); }
                for sense in rest.split(SENSE_MACRO) { senses.push(self.split_sense(sense)?); }
            }
        }

        // Forms and IPA override.
        let mut forms = None;
        let mut ipa = None;
        if parts.len() > Part::Forms as usize {
            forms = Some(self.parse_tex(&parts[Part::Forms as usize])?);
        }

        if parts.len() > Part::IPA as usize {
            ipa = Some(self.parse_tex(&parts[Part::IPA as usize])?);
        }

        // Create a canonicalised form of this entry for sorting.
        let word = word.unwrap();
        let nfkd = self.normalise_for_sort(word);
        let word = self.parse_tex(word)?;
        let plain_word = word.render_plain_text(true);

        // If requested, also add search keys.
        let mut def_search = None;
        let mut hw_search = None;
        if self.populate_search_fields {
            let mut def_search_str = match primary_definition {
                Some(ref sense) => sense.def.render_plain_text(true),
                None => String::new(),
            };

            for s in senses.iter() {
                if !def_search_str.is_empty() { def_search_str.push(' '); }
                def_search_str.push_str(&s.def.render_plain_text(true));
            }

            def_search = Some(self.normalise_for_search(&def_search_str));
            hw_search = Some(self.normalise_for_search(&plain_word));
        }

        self.entries.push(Entry {
            word,
            word_for_sorting: plain_word,
            nfkd,
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

    fn parse_tex(&self, tex: &str) -> Result<Node> {
        let mut s = Stream::new(tex);
        let mut nodes = Nodes::new();
        s.trim();
        while !s.is_empty() { self.parse_tex_content(&mut s, &mut nodes, 0)? }
        Ok(Node::group(nodes))
    }

    fn parse_tex_and_add_full_stop(&self, tex: &str) -> Result<Node> {
        // Recursively walk a node and append a full-stop to the end of
        // its last containing text node, if there is one, and if it doesn't
        // already end with a sentence delimiter (optionally followed by a
        // closing quotation mark).
        fn add_full_stop(n: &mut Node) {
            match n {
                Node::Text(t) | Node::Math(t) => {
                    if !t.as_str().trim_end_matches(&['\'', '"', '’', '”', '»', '›']).ends_with(&['.', '?', '!']) {
                        t.push('.');
                    }
                },
                Node::Macro { args, .. } | Node::Group(args) => {
                    if let Some(n) = args.last_mut() {
                        add_full_stop(n);
                    }
                },
            };
        }

        let mut node = self.parse_tex(tex)?;
        add_full_stop(&mut node);
        Ok(node)
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

            let text = s.take_until_any(&['\\', '$', '{', '}']);
            push_node(nodes, Node::text(text));

            // Process the special character.
            match s.front().unwrap_or('\0') {
                '\\' => push_node(nodes, self.parse_tex_macro(s)?),
                '$' => push_node(nodes, self.parse_tex_maths(s)?),
                '{' => {
                    s.drop_byte();
                    braces += 1;
                },
                '}' => {
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
        assert!(s.consume("{"));
        if s.consume("}") { return Ok(Node::empty()) }
        let mut children = Nodes::new();
        self.parse_tex_content(s, &mut children, 1)?;
        Ok(Node::group(children))
    }

    fn parse_tex_macro(&self, s: &mut Stream) -> Result<Node> {
        assert!(s.consume("\\"));
        if s.is_empty() { self.err("Invalid macro escape sequence")? }

        // Soft hyphen.
        if s.consume("-") { return Ok(Node::builtin(BuiltinMacro::SoftHyphen)) }

        // '\\' is invalid here.
        if s.consume("\\") { self.err("Invalid use of '\\\\'")? }

        // These are treated literally.
        if s.text().starts_with(&[' ', '&', '$', '%', '#', '{', '}']) {
            return Ok(Node::text(s.take_byte().unwrap()))
        }

        // These are unsupported single-character macros.
        if s.text().starts_with(&['!', '/', ':', '@', '[', ']', '`', '{', '~']) {
            return self.ops.handle_unknown_macro(&s.take_byte().unwrap().to_string(), vec![]).map_err(|e|
                format!("Error near line {}: {}", self.line, e)
            );
        }

        // Handle regular macros.
        let idx = s.text().find(|c| !"abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ@".contains(c));
        let macro_name = match idx {
            None => s.take_all(),
            Some(idx) => s.take_until_idx(idx)
        };

        // Handle builtin macros.
        match macro_name {
            "s" => self.parse_tex_single_arg_builtin_macro(s, BuiltinMacro::SmallCaps),
            "w" => self.parse_tex_single_arg_builtin_macro(s, BuiltinMacro::Lemma),
            "textit" => self.parse_tex_single_arg_builtin_macro(s, BuiltinMacro::Italic),
            "textbf" => self.parse_tex_single_arg_builtin_macro(s, BuiltinMacro::Bold),
            "textnf" => self.parse_tex_single_arg_builtin_macro(s, BuiltinMacro::Normal),
            "senseref" => self.parse_tex_single_arg_builtin_macro(s, BuiltinMacro::Sense),
            "Sup" => self.parse_tex_single_arg_builtin_macro(s, BuiltinMacro::Superscript),
            "Sub" => self.parse_tex_single_arg_builtin_macro(s, BuiltinMacro::Subscript),
            "par" => Ok(Node::builtin(BuiltinMacro::ParagraphBreak)),
            "ldots" => Ok(Node::builtin(BuiltinMacro::Ellipsis)),
            "this" => Ok(Node::builtin(BuiltinMacro::This)),
            "ex" | "comment" => {
                self.err(&format!("Invalid use of '\\{}'", macro_name))?;
                unreachable!();
            },
            "ref" | "label" => {
                let _ = self.parse_tex_group(s)?; // Throw away the argument.
                Ok(Node::empty())
            }
            _ => {
                let mut args = Nodes::new();
                while s.starts_with("{") { args.push(self.parse_tex_group(s)?); }
                self.ops.handle_unknown_macro(macro_name, args).map_err(|e|
                    format!("Error near line {}: {}", self.line, e)
                )
            }
        }
    }

    fn parse_tex_maths(&self, s: &mut Stream) -> Result<Node> {
        assert!(s.consume("$"));
        let node = Node::Math(s.take_until("$").to_string()); // TODO: Support maths properly.
        if !s.consume("$") { self.err("Expected '$'; if you meant to write a literal dollar sign, write '\\$'")? }
        Ok(node)
    }

    fn parse_tex_single_arg_builtin_macro(&self, s: &mut Stream, m: BuiltinMacro) -> Result<Node> {
        // Drop everything until the argument brace. We’re not a LaTeX tokeniser, so we don’t
        // support stuff like `\fract1 2`, as much as I like to write it.
        if !s.trim_start().starts_with("{") {
            self.err("Sorry, macro arguments must be enclosed in braces")?;
        }

        let arg = self.parse_tex_group(s)?;
        Ok(Node::builtin_with_args(m, vec![arg]))
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
    fn split_sense(&self, sense: &str) -> Result<Sense> {
        let mut s = Stream::new(sense);

        // Find the sense comment or first example, if any, and depending on which comes first.
        let def_str = s.take_until_either(EX_MACRO, COMMENT_MACRO);
        let def = self.parse_tex_and_add_full_stop(def_str)?;

        // If we have a comment, parse it.
        let mut comment = None;
        if s.consume(COMMENT_MACRO) {
            if def_str.is_empty() {
                self.err("\\comment is not allowed in an empty sense or empty primary definition. Use \\textit{...} instead.")?
            }

            let comment_str = s.take_until(EX_MACRO).trim();
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
                let comment_str = s.take_until(EX_MACRO).trim();
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
}

#[cfg(test)]
mod test {
    use super::*;

    struct TestOps;
    struct TestOpsWithMacroHandler;
    impl LanguageOps for TestOps {}
    impl LanguageOps for TestOpsWithMacroHandler {
        fn handle_unknown_macro(
            &self,
            macro_name: &str,
            _args: Nodes,
        ) -> Result<Node> {
            match macro_name {
                "/" => Ok(Node::text("Found /!")),
                "foo" => Ok(Node::text("BAR")),
                _ => Err(format!("Unknown macro '{}'", macro_name)),
            }
        }
    }

    #[test]
    fn test_tex_parser() {
        let t = Box::new(TestOps {});
        let g = Generator::new(t, false);

        macro_rules! check {
            ($in:literal, $out:literal) => {
                assert_eq!(g.parse_tex($in).unwrap().render(), $out);
            };
        }

        macro_rules! check_err {
            ($in:literal, $out:literal) => {
                assert_eq!(g.parse_tex($in).unwrap_err(), $out);
            };
        }

        // Plain text.
        check!("", "{\"text\":\"\"}");
        check!("aa", "{\"text\":\"aa\"}");
        check!("Sphinx of black quartz, judge my vows!", "{\"text\":\"Sphinx of black quartz, judge my vows!\"}");

        // Braces are skipped.
        check!("{}", "{\"text\":\"\"}");
        check!("a{b}c", "{\"text\":\"abc\"}");
        check!("{{a}}{b}{{c}}", "{\"text\":\"abc\"}");
        check!("{{{{{{a}}{b}{{c}}}}}}", "{\"text\":\"abc\"}");

        // Missing braces.
        check_err!("{", "Error near line 0: Unexpected end of input. Did you forget a '}}'?");
        check_err!("{{}", "Error near line 0: Unexpected end of input. Did you forget a '}}'?");
        check_err!("}", "Error near line 0: Too many '}'s!");
        check_err!("{}}", "Error near line 0: Too many '}'s!");
        check_err!("{}{", "Error near line 0: Unexpected end of input. Did you forget a '}}'?");
        check_err!("{}{}}", "Error near line 0: Too many '}'s!");

        // Maths.
        check!("$a$", "{\"math\":\"a\"}");

        // Escaping braces.
        check!("\\{", "{\"text\":\"{\"}");
        check!("\\}", "{\"text\":\"}\"}");
        check!("{\\{}", "{\"text\":\"{\"}");
        check!("{\\}}", "{\"text\":\"}\"}");
        check!("\\{{}", "{\"text\":\"{\"}");
        check!("\\}{}", "{\"text\":\"}\"}");

        // Single-character macros.
        check!("\\-", "{\"macro\":{\"name\":\"soft_hyphen\"}}");
        check!("\\ X", "{\"text\":\" X\"}"); // 'X' is required because we trim whitespace.
        check!("\\&", "{\"text\":\"&\"}");
        check!("\\$", "{\"text\":\"$\"}");
        check!("\\%", "{\"text\":\"%\"}");
        check!("\\#", "{\"text\":\"#\"}");
        check!("{\\-}", "{\"macro\":{\"name\":\"soft_hyphen\"}}");
        check!("{\\ }", "{\"text\":\" \"}");
        check!("{\\&}", "{\"text\":\"&\"}");
        check!("{\\$}", "{\"text\":\"$\"}");
        check!("{\\%}", "{\"text\":\"%\"}");
        check!("{\\#}", "{\"text\":\"#\"}");

        // Unsupported single-character macros.
        check_err!("\\@", "Error near line 0: Unsupported macro '\\@'. Please add support for it to the dictionary generator.");
        check_err!("\\[", "Error near line 0: Unsupported macro '\\['. Please add support for it to the dictionary generator.");
        check_err!("\\]", "Error near line 0: Unsupported macro '\\]'. Please add support for it to the dictionary generator.");

        // Missing macro name.
        check_err!("\\", "Error near line 0: Invalid macro escape sequence");

        // Special macros aren't normally valid.
        check_err!("\\\\", "Error near line 0: Invalid use of '\\\\'");
        check_err!("\\ex", "Error near line 0: Invalid use of '\\ex'");
        check_err!("\\comment", "Error near line 0: Invalid use of '\\comment'");

        // Builtin single-argument macros.
        check!("\\s{a}{b}", "{\"group\":[{\"macro\":{\"name\":\"small_caps\",\"args\":[{\"text\":\"a\"}]}},{\"text\":\"b\"}]}");
        check!("\\s{a{c}}{b}", "{\"group\":[{\"macro\":{\"name\":\"small_caps\",\"args\":[{\"text\":\"ac\"}]}},{\"text\":\"b\"}]}");
        check!("\\s{a{\\s{c}}}{b}", "{\"group\":[{\"macro\":{\"name\":\"small_caps\",\"args\":[{\"group\":[{\"text\":\"a\"},{\"macro\":{\"name\":\"small_caps\",\"args\":[{\"text\":\"c\"}]}}]}]}},{\"text\":\"b\"}]}");
        check!("\\Sup{foo}bar", "{\"group\":[{\"macro\":{\"name\":\"superscript\",\"args\":[{\"text\":\"foo\"}]}},{\"text\":\"bar\"}]}");
        check!("\\Sub{foo}bar", "{\"group\":[{\"macro\":{\"name\":\"subscript\",\"args\":[{\"text\":\"foo\"}]}},{\"text\":\"bar\"}]}");

        // Builtin macros w/ no arguments.
        check!("\\par", "{\"macro\":{\"name\":\"paragraph_break\"}}");
        check!("\\ldots", "{\"macro\":{\"name\":\"ellipsis\"}}");
        check!("\\this", "{\"macro\":{\"name\":\"this\"}}");

        // Labels and refs are dropped.
        check!("x\\ref{...abab{\\w{ss}}}y", "{\"text\":\"xy\"}");
        check!("x\\label{...abab{\\w{ss}}}y", "{\"text\":\"xy\"}");
    }

    #[test]
    fn test_macro_handler() {
        let t = Box::new(TestOpsWithMacroHandler {});
        let g = Generator::new(t, false);

        // Unknown macros are passed to the lang ops.
        assert_eq!(g.parse_tex("\\/").unwrap().render(), "{\"text\":\"Found /!\"}");
        assert_eq!(g.parse_tex("\\foo").unwrap().render(), "{\"text\":\"BAR\"}");
    }

    #[test]
    fn test_entry_parser() {
        macro_rules! check {
            ($in:literal, $out:literal) => {
                {
                    let t = Box::new(TestOps {});
                    let mut g = Generator::new(t, true);
                    g.parse($in).unwrap();

                    // Use json::parse() on both sides so we can format the expected JSON output
                    // in a way that is actually legible while still comparing the json without
                    // having to care about whitespace.
                    assert_eq!(
                        json::parse(&g.json()).unwrap().to_string(),
                        json::parse($out).unwrap().to_string(),
                    );
                }
            };
        }

        check!("x|y|z|q", r#"
            {
                "entries": [
                    {
                        "word": {
                            "text": "x"
                        },
                        "pos": {
                            "text": "y"
                        },
                        "etym": {
                            "text": "z"
                        },
                        "primary_definition": {
                        "def": {
                            "text": "q."
                            }
                        },
                        "hw_search": "x",
                        "def_search": "q"
                    }
                ]
            }
        "#);

        check!("a&b|||c&d", r#"
            {
                "entries": [
                    {
                        "word": {
                            "text": "a&b"
                        },
                        "pos": {
                            "text": ""
                        },
                        "etym": {
                            "text": ""
                        },
                        "primary_definition": {
                            "def": {
                                "text": "c&d."
                            }
                        },
                        "hw_search": "ab",
                        "def_search": "cd"
                    }
                ]
            }
        "#);

        check!("q|||mc d e g x y e mm ma mb mq", r#"
            {
                "entries": [
                    {
                        "word": {
                            "text": "q"
                        },
                        "pos": {
                            "text": ""
                        },
                        "etym": {
                            "text": ""
                        },
                        "primary_definition": {
                            "def": {
                                "text": "mc d e g x y e mm ma mb mq."
                            }
                        },
                        "hw_search": "q",
                        "def_search": "d e g ma mb mc mm mq x y"
                    }
                ]
            }
        "#);

        check!("aub’heír\\Sup{L}|v. (in)tr.|obéir|To obey (+\\s{part} sbd.)", r#"
            {
                "entries": [
                    {
                        "word": {
                            "group": [
                                {
                                    "text": "aub’heír"
                                },
                                {
                                    "macro": {
                                        "name": "superscript",
                                        "args": [
                                            {
                                                "text": "L"
                                            }
                                        ]
                                    }
                                }
                            ]
                        },
                        "pos": {
                            "text": "v. (in)tr."
                        },
                        "etym": {
                            "text": "obéir"
                        },
                        "primary_definition": {
                            "def": {
                                "group": [
                                    {
                                        "text": "To obey (+"
                                    },
                                    {
                                        "macro": {
                                            "name": "small_caps",
                                            "args": [
                                                {
                                                    "text": "part"
                                                }
                                            ]
                                        }
                                    },
                                    {
                                        "text": " sbd.)."
                                    }
                                ]
                            }
                        },
                        "hw_search": "aubheir",
                        "def_search": "obey sbd to"
                    }
                ]
            }
        "#);

        check!("ánvé|v. tr.|animer|+\\s{acc} To bring to life, animate", r#"
            {
                "entries": [
                    {
                        "word": {
                            "text": "ánvé"
                        },
                        "pos": {
                            "text": "v. tr."
                        },
                        "etym": {
                            "text": "animer"
                        },
                        "primary_definition": {
                            "def": {
                                "group": [
                                    {
                                        "text": "+"
                                    },
                                    {
                                        "macro": {
                                            "name": "small_caps",
                                            "args": [
                                                {
                                                    "text": "acc"
                                                }
                                            ]
                                        }
                                    },
                                    {
                                        "text": " To bring to life, animate."
                                    }
                                ]
                            }
                        },
                        "hw_search": "anve",
                        "def_search": "animate bring life to"
                    }
                ]
            }
        "#);

        check!("A|B|C|D\\\\ E\\comment F\\ex G\\comment H", r#"
            {
                "entries": [
                    {
                        "word": {
                            "text": "A"
                        },
                        "pos": {
                            "text": "B"
                        },
                        "etym": {
                            "text": "C"
                        },
                        "primary_definition": {
                            "def": {
                                "text": "D."
                            }
                        },
                        "senses": [
                            {
                                "def": {
                                    "text": "E."
                                },
                                "comment": {
                                    "text": "F."
                                },
                                "examples": [
                                    {
                                        "text": {
                                            "text": "G."
                                        },
                                        "comment": {
                                            "text": "H."
                                        }
                                    }
                                ]
                            }
                        ],
                        "hw_search": "a",
                        "def_search": "d e"
                    }
                ]
            }
        "#);

        check!("a|b|c|\\\\d", r#"
            {
                "entries": [
                    {
                        "word": {
                            "text": "a"
                        },
                        "pos": {
                            "text": "b"
                        },
                        "etym": {
                            "text": "c"
                        },
                        "senses": [
                            {
                                "def": {
                                    "text": "d."
                                }
                            }
                        ],
                        "hw_search": "a",
                        "def_search": "d"
                    }
                ]
            }
        "#);

        check!("a > b", r#"
            {
                "entries": [
                    {
                        "word": {
                            "text": "a"
                        },
                        "refs": [
                            { "text": "b" }
                        ],
                        "search": "a"
                    }
                ]
            }
        "#);

        check!("a > b, cd , ef  ", r#"
            {
                "entries": [
                    {
                        "word": {
                            "text": "a"
                        },
                        "refs": [
                            { "text": "b" },
                            { "text": "cd" },
                            { "text": "ef" }
                        ],
                        "search": "a"
                    }
                ]
            }
        "#);

        check!("ac’hes > \\w{a} + \\w{c’hes}", r#"
            {
                "entries": [
                    {
                        "word": {
                            "text": "ac’hes"
                        },
                        "refs": [
                            {
                                "group": [
                                    {
                                        "macro": {
                                            "name": "lemma",
                                            "args": [
                                                {
                                                    "text": "a"
                                                }
                                            ]
                                        }
                                    },
                                    {
                                        "text": " + "
                                    },
                                    {
                                        "macro": {
                                            "name": "lemma",
                                            "args": [
                                                {
                                                    "text": "c’hes"
                                                }
                                            ]
                                        }
                                    }
                                ]
                            }
                        ],
                        "search": "aches"
                    }
                ]
            }
        "#);

        check!(
            r#"vê₃|v.|même|
                \\ To be the same, identical, alike
                \\ \textit{(emphatic)} Oneself
                    \comment Placed either directly after a noun or infixed after (the prefix part of) a pronoun.
                    \ex \w{Aúłau vê ssèhá’z ivúb’hvâ} ‘Time itself stood still’
                        \comment Lit. ‘Time itself ceased its movement’
                    \ex \w{Jvêsyráré} ‘I saw it myself’
                \\ \w{vêvâ} Even
                    \comment In this sense, \w{vêvâ} usually precedes the noun (phrase) it qualifies.
                    \ex \s{H. P. Lovecraft:} \w{Lavúrer’sý’ýâ là dwájávé sdaúr~/ Ádȅr trâ vêvâ Dérny’éhuf laúv’raú.}
                        ‘That is not dead which can eternal lie~/ And with strange æons even death may die’."#,
            r#"
                {
                    "entries": [
                        {
                            "word": {
                                "text": "vê₃"
                            },
                            "pos": {
                                "text": "v."
                            },
                            "etym": {
                                "text": "même"
                            },
                            "senses": [
                                {
                                    "def": {
                                        "text": "To be the same, identical, alike."
                                    }
                                },
                                {
                                    "def": {
                                        "group": [
                                            {
                                                "macro": {
                                                    "name": "italic",
                                                    "args": [
                                                        {
                                                            "text": "(emphatic)"
                                                        }
                                                    ]
                                                }
                                            },
                                            {
                                                "text": " Oneself."
                                            }
                                        ]
                                    },
                                    "comment": {
                                        "text": "Placed either directly after a noun or infixed after (the prefix part of) a pronoun."
                                    },
                                    "examples": [
                                        {
                                            "text": {
                                                "group": [
                                                    {
                                                        "macro": {
                                                            "name": "lemma",
                                                            "args": [
                                                                {
                                                                    "text": "Aúłau vê ssèhá’z ivúb’hvâ"
                                                                }
                                                            ]
                                                        }
                                                    },
                                                    {
                                                        "text": " ‘Time itself stood still’."
                                                    }
                                                ]
                                            },
                                            "comment": {
                                                "text": "Lit. ‘Time itself ceased its movement’."
                                            }
                                        },
                                        {
                                            "text": {
                                                "group": [
                                                    {
                                                        "macro": {
                                                            "name": "lemma",
                                                            "args": [
                                                                {
                                                                    "text": "Jvêsyráré"
                                                                }
                                                            ]
                                                        }
                                                    },
                                                    {
                                                        "text": " ‘I saw it myself’."
                                                    }
                                                ]
                                            }
                                        }
                                    ]
                                },
                                {
                                    "def": {
                                        "group": [
                                            {
                                                "macro": {
                                                    "name": "lemma",
                                                    "args": [
                                                        {
                                                            "text": "vêvâ"
                                                        }
                                                    ]
                                                }
                                            },
                                            {
                                                "text": " Even."
                                            }
                                        ]
                                    },
                                    "comment": {
                                        "group": [
                                            {
                                                "text": "In this sense, "
                                            },
                                            {
                                                "macro": {
                                                    "name": "lemma",
                                                    "args": [
                                                        {
                                                            "text": "vêvâ"
                                                        }
                                                    ]
                                                }
                                            },
                                            {
                                                "text": " usually precedes the noun (phrase) it qualifies."
                                            }
                                        ]
                                    },
                                    "examples": [
                                        {
                                            "text": {
                                                "group": [
                                                    {
                                                        "macro": {
                                                            "name": "small_caps",
                                                            "args": [
                                                                {
                                                                    "text": "H. P. Lovecraft:"
                                                                }
                                                            ]
                                                        }
                                                    },
                                                    {
                                                        "text": " "
                                                    },
                                                    {
                                                        "macro": {
                                                            "name": "lemma",
                                                            "args": [
                                                                {
                                                                    "text": "Lavúrer’sý’ýâ là dwájávé sdaúr~/ Ádȅr trâ vêvâ Dérny’éhuf laúv’raú."
                                                                }
                                                            ]
                                                        }
                                                    },
                                                    {
                                                        "text": " ‘That is not dead which can eternal lie~/ And with strange æons even death may die’."
                                                    }
                                                ]
                                            }
                                        }
                                    ]
                                }
                            ],
                            "hw_search": "ve",
                            "def_search": "alike be even identical oneself same the to"
                        }
                    ]
                }
            "#
        );

        check!("Z|b|c|d\nX>Y\nY|e|f|g", r#"
            {
                "entries": [
                    {
                        "word": {
                            "text": "X"
                        },
                        "refs": [
                            {
                                "text": "Y"
                            }
                        ],
                        "search": "x"
                    },
                    {
                        "word": {
                            "text": "Y"
                        },
                        "pos": {
                            "text": "e"
                        },
                        "etym": {
                            "text": "f"
                        },
                        "primary_definition": {
                            "def": {
                                "text": "g."
                            }
                        },
                        "hw_search": "y",
                        "def_search": "g"
                    },
                    {
                        "word": {
                            "text": "Z"
                        },
                        "pos": {
                            "text": "b"
                        },
                        "etym": {
                            "text": "c"
                        },
                        "primary_definition": {
                            "def": {
                                "text": "d."
                            }
                        },
                        "hw_search": "z",
                        "def_search": "d"
                    }
                ]
            }

        "#);
    }

    #[test]
    fn test_entry_parser_errors() {
        let t = Box::new(TestOps {});
        let mut g = Generator::new(t, true);

        macro_rules! check_err {
            ($in:literal, $out:literal) => {
                assert_eq!(g.parse($in).unwrap_err(), $out);
            };
        }

        check_err!(
            "x|y|z|\\comment abcd",
            "Error near line 1: \\comment is not allowed in an empty sense or empty primary definition. Use \\textit{...} instead."
        );

        check_err!(
            "x|y|z|\\\\\\comment abcd",
            "Error near line 1: \\comment is not allowed in an empty sense or empty primary definition. Use \\textit{...} instead."
        );

        check_err!(
            "x|y|z|\\ex abcd",
            "Error near line 1: \\ex is not allowed in an empty sense or empty primary definition."
        );

        check_err!(
            "x|y|z|\\\\\\ex abcd",
            "Error near line 1: \\ex is not allowed in an empty sense or empty primary definition."
        );

        check_err!(
            "\\\\a|||",
            "Error near line 1: '\\\\' cannot be used in the lemma"
        );

        check_err!(
            "\\comment|||",
            "Error near line 1: '\\comment' cannot be used in the lemma"
        );

        check_err!(
            "\\ex|||",
            "Error near line 1: '\\ex' cannot be used in the lemma"
        );

        check_err!(
            "foo",
            "Error near line 1: An entry must contain at least one '|' or '>'"
        );

        check_err!(
            "\\comment > b",
            "Error near line 1: '\\comment' cannot be used in a reference entry"
        );

        check_err!(
            "a > \\comment",
            "Error near line 1: '\\comment' cannot be used in a reference entry"
        );

        check_err!(
            "\\ex > b",
            "Error near line 1: '\\ex' cannot be used in a reference entry"
        );

        check_err!(
            "a > \\ex",
            "Error near line 1: '\\ex' cannot be used in a reference entry"
        );

        check_err!(
            "\\\\ > b",
            "Error near line 1: '\\\\' cannot be used in a reference entry"
        );

        check_err!(
            "a > \\\\",
            "Error near line 1: '\\\\' cannot be used in a reference entry"
        );
    }
}
