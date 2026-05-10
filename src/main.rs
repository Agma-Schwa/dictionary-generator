use std::fs::read_to_string;
use dictgen::Options;
use clap::Parser;

#[derive(Parser)]
struct Args {
    /// Dictionary file to parse.
    #[arg()] file: String,

    /// Text to convert to IPA; suppresses dictionary output.
    #[arg(long, default_value = None)] convert: Option<String>,

    /// Whether to minify the JSON output.
    #[arg(long, default_value_t = false)] minify: bool,

    /// Whether to always include IPA in the output.
    #[arg(long, default_value_t = false)] ipa: bool,

    /// Whether to populate fields used for searching in the frontend.
    #[arg(long, default_value_t = false)] search: bool,
}

pub fn main() {
    let args = Args::parse();
    let mut g = dictgen::generator::Generator::new(Options {
        populate_search_fields: args.search,
        always_include_ipa: args.ipa,
        pretty_json: !args.minify
    });

    let text = read_to_string(args.file).unwrap();
    g.parse(&text).unwrap();

    if let Some(conv) = args.convert {
        let node = g.to_ipa(&conv).unwrap().unwrap();
        println!("{}", node.render_plain_text(true));
        return;
    }

    println!("{}", g.json());
}
