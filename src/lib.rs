use serde::Serialize;
use smallstr::SmallString;

pub mod generator;
mod string_utils;
mod wasm;

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
    Reference,
    This, // The current word.
}

pub type Nodes = Vec<Node>;
type NodeText = SmallString<[u8; 16]>;

/// A node that represents formatting or text.
#[derive(Serialize, Debug, Clone)]
#[serde(rename_all = "snake_case")]
pub enum Node {
    /// Text node.
    Text(NodeText),

    /// Math node.
    Math(NodeText),

    /// A group of nodes to be rendered directly adjacent to one another.
    Group(Nodes),

    /// A builtin macro; all renderers are expected to support all builtin macros.
    Macro {
        name: BuiltinMacro,
        #[serde(skip_serializing_if = "Nodes::is_empty")] args: Nodes
    },

    /// Projects may define their own custom macros.
    CustomMacro {
        name: NodeText,
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

// You can't define a function that is generic on mutability, so the sledgehammer
// approach will have to do.
macro_rules! define_node_traversal_function {
    ($name:ident, $node:ty, $arg:ty $(, $mutability:tt)?) => {
        fn $name<F: FnMut($arg)>(n: $node, callback: &mut F, visit_macro_args: bool) {
            match n {
                Node::Text(_) | Node::Math(_) => callback(n),
                Node::Group(args) => {
                    for a in args { $name(a, callback, visit_macro_args) }
                }
                Node::Macro { args, .. } | Node::CustomMacro { args, .. } => {
                    if visit_macro_args {
                        for a in args { $name(a, callback, visit_macro_args) }
                    }
                }
            }
        }
    };
}

define_node_traversal_function!(_traverse_text_node, &Node, &Node);
define_node_traversal_function!(_traverse_text_node_mut, &mut Node, &mut Node, mut);

impl Node {
    pub fn builtin(m: BuiltinMacro) -> Node {
        Node::Macro { name: m, args: vec![] }
    }

    pub fn builtin_with_args(m: BuiltinMacro, args: Nodes) -> Node {
        // We don't try and fold builtin macros because no-one should actually
        // ever write e.g. nested \textit{} in practice.
        Node::Macro { name: m, args }
    }

    pub fn custom(m: &str, args: Nodes) -> Node {
        Node::CustomMacro { name: m.into(), args }
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

    pub fn render_plain_text(&self, strip_macros: bool) -> NodeText {
        let mut s = NodeText::new();
        self.visit_text_nodes(!strip_macros, |t| s.push_str(t.unwrap_text()));
        s
    }

    pub fn text(s: impl Into<SmallString<[u8; 16]>>) -> Node {
        Node::Text(s.into())
    }

    pub fn unwrap_text(&self) -> &str {
        match self {
            Node::Text(t) | Node::Math(t) => t,
            _ => panic!("Not a text node!"),
        }
    }

    pub fn visit_text_nodes<F: FnMut(&Node)>(&self, visit_macro_args: bool, mut f: F) {
        _traverse_text_node(self, &mut f, visit_macro_args);
    }

    pub fn visit_text_nodes_mut<F: FnMut(&mut Node)>(&mut self, visit_macro_args: bool, mut f: F) {
        _traverse_text_node_mut(self, &mut f, visit_macro_args);
    }
}

#[derive(Debug, Clone, Copy)]
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

/// Parse a string as a dictionary file and return the JSON string.
pub fn parse_and_generate(
    input: &str,
    opts: Options,
) -> Result<String> {
    let mut g = generator::Generator::new(opts);
    g.parse(input)?;
    Ok(g.json())
}
