use std::{borrow::Cow, cmp::Ordering};
use serde::Serialize;

mod generator;
mod string_utils;

pub type Result<T> = std::result::Result<T, String>;

#[derive(Serialize, Debug, Clone, Copy, PartialEq, Eq)]
#[serde(rename_all = "snake_case")]
pub enum BuiltinMacro {
    Bold,
    Ellipsis,
    Italic,
    Lemma, // Formatting used for the headword
    Normal, // Remove all formatting.
    ParagraphBreak,
    Sense,
    SmallCaps,
    Subscript,
    Superscript,
    SoftHyphen,
    This, // The current word.
}

pub type Nodes = Vec<Node>;

/// A node that represents formatting or text.
#[derive(Serialize, Debug, Clone)]
#[serde(rename_all = "snake_case")]
pub enum Node {
    Text(String),
    Math(String),
    Group(Nodes),
    Macro {
        name: BuiltinMacro,
        #[serde(skip_serializing_if = "Nodes::is_empty")] args: Nodes
    },
}

#[derive(Copy, Clone, Debug, Default)]
pub struct Options {
    /// Whether to populate search fields in the JSON output.
    pub populate_search_fields: bool,

    /// Whether to always include IPA, even if the user didn’t provide any.
    pub always_include_ipa: bool,

    /// Pretty-print JSON.
    pub pretty_json: bool,
}

impl Node {
    pub fn builtin(m: BuiltinMacro) -> Node {
        Node::Macro { name: m, args: vec![] }
    }

    pub fn builtin_with_args(m: BuiltinMacro, args: Nodes) -> Node {
        // We don't try and fold builtin macros because no-one should actually
        // ever write e.g. nested \textit{} in practice.
        Node::Macro { name: m, args }
    }

    pub fn group(mut children: Nodes) -> Node {
        if children.is_empty() { Self::empty() }
        else if children.len() == 1 { children.pop().unwrap() }
        else {Node::Group(children) }
    }

    pub fn empty() -> Node {
        Self::text("")
    }

    pub fn is_builtin_macro(&self, m: BuiltinMacro) -> bool {
        match self {
            Node::Macro { name, .. } => *name == m,
            _ => false
        }
    }

    pub fn render(&self) -> String {
        serde_json::to_string(self).unwrap()
    }

    pub fn render_plain_text(&self, strip_macros: bool) -> String {
        fn render_impl(s: &mut String, n: &Node, strip_macros: bool) {
            match n {
                Node::Text(t) | Node::Math(t) => s.push_str(t),
                Node::Group(args) => {
                    for a in args { render_impl(s, a, strip_macros) }
                }
                Node::Macro { args, .. } => {
                    if !strip_macros {
                        for a in args { render_impl(s, a, strip_macros) }
                    }
                }
            }
        }

        let mut s = String::new();
        render_impl(&mut s, self, strip_macros);
        s
    }

    pub fn text(s: impl Into<String>) -> Node {
        Node::Text(s.into())
    }
}

pub enum Part {
    /// Part of speech.
    POS,

    /// Etymology.
    Etym,

    /// Definition (senses and examples).
    Def,

    /// Forms (optional).
    Forms,

    /// IPA override (optional).
    IPA,

    /// Not an actual field but rather the maximum number of valid fields.
    Max,
}

/// Trait that should be customised for each 'language' i.e. dictionary implementation;
/// this is the main customisation point of the generator.
pub trait LanguageOps {
    /// Sort headwords. Should return 'true' if 'a' is to be sorted before 'b'.
    fn collate(&self, a: &str, b: &str, a_nfkd: &str, b_nfkd: &str) -> Ordering {
        if a_nfkd == b_nfkd { a.cmp(b) }
        else { a_nfkd.cmp(b_nfkd) }
    }

    /// Invoked on any unknown macro. 'macro_name' does not include the backslash.
    fn handle_unknown_macro(
        &self,
        macro_name: &str,
        _args: Nodes,
    ) -> Result<Node> {
        Err(format!("Unsupported macro '\\{}'. Please add support for it to the dictionary generator.", macro_name))
    }

    /// Preprocess the fields before conversion/parsing is attempted.
    fn preprocess_full_entry(&self, _entry: &mut [Cow<'_, str>]) -> Result<()> {
        Ok(())
    }

    /// Convert a language’s text to IPA.
    fn to_ipa(&self, _word: &str) -> Result<Option<Node>> {
        Ok(None)
    }
}

/// Parse a string as a dictionary file and return the JSON string.
pub fn parse_and_generate(
    ops: Box<dyn LanguageOps>,
    input: &str,
    opts: Options,
) -> Result<String> {
    let mut g = generator::Generator::new(ops, opts);
    g.parse(input)?;
    Ok(g.json())
}
