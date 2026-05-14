use clap::Parser;
use dictgen::Options;
use std::fs::read_to_string;
use std::io::{stderr, IsTerminal};
use std::process::exit;

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

    /// Stop after a single error.
    #[arg(long, default_value_t = false)] stop_at_err: bool,
}

pub fn main() {
    let args = Args::parse();
    let mut g = dictgen::generator::Generator::new(Options {
        populate_search_fields: args.search,
        always_include_ipa: args.ipa,
        pretty_json: !args.minify,
        colour: stderr().is_terminal(),
        keep_parsing: !args.stop_at_err
    });

    let text = read_to_string(&args.file).unwrap();
    if let Err(e) = g.parse(&text, &args.file) {
        eprint!("{}", e);
        exit(1);
    }


    if let Some(conv) = args.convert {
        let node = match g.to_ipa(&conv) {
            Ok(node) => node,
            Err(e) => {
                eprint!("{}", e);
                exit(1);
            }
        };

        let node = node.unwrap();
        println!("{}", node.render_plain_text(true));
        return;
    }

    println!("{}", g.json());
}
