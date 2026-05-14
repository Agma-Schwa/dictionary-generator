#[cfg(test)]
mod test {
    use super::*;

    fn dedent(s: &str) -> String {
        let indent = s.split('\n')
            .filter(|line| !line.trim().is_empty())
            .map(|line| line.len() - line.trim_start().len())
            .min()
            .unwrap_or(0);

        let mut res = String::new();
        for line in s.trim().split('\n') {
            res.push('\n');
            if line.trim().is_empty() { continue; }
            let start = indent.min(line.len() - line.trim_start().len());
            res.push_str(&line[start..]);
        }

        res.trim().to_string()
    }

    // The way in which assert_eq!() prints its inputs isn't suitable for us, so we roll our own.
    macro_rules! assert_eq {
        ($left:expr, $right:expr $(,)?) => {
            match (&$left, &$right) {
                (left, right) => {
                    let right = dedent(right);
                    if !(left.trim() == right) {
                        panic!(
                            "EQ failed:\n==== LEFT ====\n{}\n==== RIGHT ====\n{}\n==== END ====",
                            left.trim(),
                            right
                        )
                    }
                }
            }
        };
    }

    macro_rules! assert_contains {
        ($left:expr, $right:expr $(,)?) => {
            match (&$left, &$right) {
                (left, right) => {
                    let right = dedent(right);
                    if !(left.contains(&right)) {
                        panic!(
                            "CONTAINS failed:\n==== LEFT ====\n{}\n==== RIGHT ====\n{}\n==== END ====",
                            left.trim(),
                            right
                        )
                    }
                }
            }
        };
    }

    #[test]
    fn test_tex_parser() {
        macro_rules! check {
            ($in:literal, $out:literal) => {
                assert_eq!(TeXParser::parse($in, &[], false).unwrap().render(), $out);
            };
        }

        macro_rules! check_err {
            ($in:literal, $out:literal) => {
                assert_contains!(TeXParser::parse($in, &[], false).unwrap_err(), $out);
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
        check_err!("{", "Unexpected end of input. Did you forget a '}'?");
        check_err!("{{}", "Unexpected end of input. Did you forget a '}'?");
        check_err!("}", "Too many '}'s!");
        check_err!("{}}", "Too many '}'s!");
        check_err!("{}{", "Unexpected end of input. Did you forget a '}'?");
        check_err!("{}{}}", "Too many '}'s!");

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
        check_err!("\\@", "Unknown macro '\\@'; did you forget to '$declare @ 0' somewhere?");
        check_err!("\\[", "Unknown macro '\\['; did you forget to '$declare [ 0' somewhere?");
        check_err!("\\]", "Unknown macro '\\]'; did you forget to '$declare ] 0' somewhere?");

        // Missing macro name.
        check_err!("\\", "Invalid macro escape sequence");

        // Special macros aren't normally valid.
        check_err!("\\\\", "'\\\\' cannot be used in this field");
        check_err!("\\ex", "'\\ex' cannot be used in this field");
        check_err!("\\comment", "'\\comment' cannot be used in this field");

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

        // Labels are dropped.
        check!("x\\label{...abab{\\w{ss}}}y", "{\"text\":\"xy\"}");

        // References are preserved.
        check!("x\\ref{abab}y", "{\"group\":[{\"text\":\"x\"},{\"macro\":{\"name\":\"reference\",\"args\":[{\"text\":\"abab\"}]}},{\"text\":\"y\"}]}");

        // Unicode escape sequence.
        check!("\\x{de}", "{\"text\":\"Þ\"}");
        check_err!("\\x", "Expected '{' after '\\x'");
        check_err!("\\x{", "Missing '}' in Unicode escape sequence");
        check_err!("\\x{}", "Expected at least 1 digit in '\\x{...}'");
        check_err!("\\x{100000000}", "Invalid unicode codepoint '100000000'");
        check_err!("\\x{q}", "Invalid unicode codepoint 'q'");

        // Whitespace folding.
        check!("  \n\r\t a b    c \n\t\r\n\t  d  \n \r \t", "{\"text\":\"a b c d\"}");
    }

    #[test]
    fn test_declare_directive() {
        let custom_macros = vec![
            CustomMacroDecl { name: "foo".into(), args: 0, loc: SourceRange(None) },
            CustomMacroDecl { name: "bar".into(), args: 1, loc: SourceRange(None) },
        ];

        macro_rules! check {
            ($in:literal, $out:literal) => {
                let node = TeXParser::parse($in, &custom_macros, false).unwrap().render();
                assert_eq!(
                    json::parse(&node).unwrap().to_string(),
                    json::parse($out).unwrap().to_string()
                );
            };
        }

        macro_rules! check_err {
            ($in:literal, $out:literal) => {
                let res = TeXParser::parse($in, &custom_macros, false);
                assert_contains!(res.unwrap_err().to_string(), $out);
            };
        }

        check!("\\foo", "{\"custom_macro\":{\"name\":\"foo\"}}");
        check!("\\bar{xyz}", "{\"custom_macro\":{\"name\":\"bar\",\"args\":[{\"text\":\"xyz\"}]}}");

        check_err!("\\bar", "Macro '\\bar' expects 1 argument, but got 0");
        check_err!("\\bar{123}{234}", "Macro '\\bar' expects 1 argument, but got 2");
        check_err!("\\foo{xyz}", "Macro '\\foo' expects 0 arguments, but got 1");
        check_err!("\\foo{xyz}{abc}", "Macro '\\foo' expects 0 arguments, but got 2");
    }

    macro_rules! check_with_generator {
        ($g:expr, $in: literal, $out: literal) => {
            $g.parse($in, "<input>").unwrap();

            // Use json::parse() on both sides so we can format the expected JSON output
            // in a way that is actually legible while still comparing the json without
            // having to care about whitespace.
            assert_eq!(
                json::parse(&$g.json()).unwrap().to_string(),
                json::parse($out).unwrap().to_string(),
            );
        };
    }

    #[test]
    fn test_entry_parser() {
        macro_rules! check {
            ($in:literal, $out:literal) => {
                {
                    let mut g = Generator::new(Options { populate_search_fields: true, ..Default::default() });
                    check_with_generator!(g, $in, $out);
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
                        "word": { "text": "a" },
                        "ref": { "text": "b" },
                        "search": "a"
                    }
                ]
            }
        "#);

        check!("a, c ,d ,  b > e", r#"
            {
                "entries": [
                    {
                        "word": {
                            "text": "a"
                        },
                        "ref": {
                            "text": "e"
                        },
                        "search": "a"
                    },
                    {
                        "word": {
                            "text": "b"
                        },
                        "ref": {
                            "text": "e"
                        },
                        "search": "b"
                    },
                    {
                        "word": {
                            "text": "c"
                        },
                        "ref": {
                            "text": "e"
                        },
                        "search": "c"
                    },
                    {
                        "word": {
                            "text": "d"
                        },
                        "ref": {
                            "text": "e"
                        },
                        "search": "d"
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
                        "ref": {
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
                        },
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
                        "ref": {
                            "text": "Y"
                        },
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

        check!("a , > b", r#"
            {
                "entries": [
                    {
                        "word": {
                            "text": "a"
                        },
                        "ref": {
                            "text": "b"
                        },
                        "search": "a"
                    }
                ]
            }
        "#);

        check!("ad’hór|v. tr. or n.|adore|love\nvy’í, aúsó > eḍ", r#"
            {
                "entries": [
                    {
                        "word": {
                            "text": "ad’hór"
                        },
                        "pos": {
                            "text": "v. tr. or n."
                        },
                        "etym": {
                            "text": "adore"
                        },
                        "primary_definition": {
                            "def": {
                                "text": "love."
                            }
                        },
                        "hw_search": "adhor",
                        "def_search": "love"
                    },
                    {
                        "word": {
                            "text": "aúsó"
                        },
                        "ref": {
                            "text": "eḍ"
                        },
                        "search": "auso"
                    },
                    {
                        "word": {
                            "text": "vy’í"
                        },
                        "ref": {
                            "text": "eḍ"
                        },
                        "search": "vyi"
                    }
                ]
            }
        "#);

        check!(" b|x|x|x\na|y|y|y", r#"
            {
                "entries": [
                    {
                        "word": {
                            "text": "a"
                        },
                        "pos": {
                            "text": "y"
                        },
                        "etym": {
                            "text": "y"
                        },
                        "primary_definition": {
                            "def": {
                                "text": "y."
                            }
                        },
                        "hw_search": "a",
                        "def_search": "y"
                    },
                    {
                        "word": {
                            "text": "b"
                        },
                        "pos": {
                            "text": "x"
                        },
                        "etym": {
                            "text": "x"
                        },
                        "primary_definition": {
                            "def": {
                                "text": "x."
                            }
                        },
                        "hw_search": "b",
                        "def_search": "x"
                    }
                ]
            }
        "#);

        // Test that full-stop insertion works properly in the presence of macros with no
        // arguments or \ref.
        check!("a|b|c|foo \\ref{bar}", r#"
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
                        "primary_definition": {
                            "def": {
                                "group": [
                                    {
                                        "text": "foo "
                                    },
                                    {
                                        "macro": {
                                            "name": "reference",
                                            "args": [
                                                {
                                                    "text": "bar"
                                                }
                                            ]
                                        }
                                    },
                                    {
                                        "text": "."
                                    }
                                ]
                            }
                        },
                        "hw_search": "a",
                        "def_search": "foo"
                    }
                ]
            }
        "#);

        check!("a|b|c|foo \\ref{bar}.", r#"
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
                        "primary_definition": {
                            "def": {
                                "group": [
                                    {
                                        "text": "foo "
                                    },
                                    {
                                        "macro": {
                                            "name": "reference",
                                            "args": [
                                                {
                                                    "text": "bar"
                                                }
                                            ]
                                        }
                                    },
                                    {
                                        "text": "."
                                    }
                                ]
                            }
                        },
                        "hw_search": "a",
                        "def_search": "foo"
                    }
                ]
            }
        "#);

        check!("a|b|c|foo \\ref{bar.}.", r#"
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
                        "primary_definition": {
                            "def": {
                                "group": [
                                    {
                                        "text": "foo "
                                    },
                                    {
                                        "macro": {
                                            "name": "reference",
                                            "args": [
                                                {
                                                    "text": "bar."
                                                }
                                            ]
                                        }
                                    },
                                    {
                                        "text": "."
                                    }
                                ]
                            }
                        },
                        "hw_search": "a",
                        "def_search": "foo"
                    }
                ]
            }
        "#);

        check!("a|b|c|foo \\this", r#"
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
                        "primary_definition": {
                            "def": {
                                "group": [
                                    {
                                        "text": "foo "
                                    },
                                    {
                                        "macro": {
                                            "name": "this"
                                        }
                                    },
                                    {
                                        "text": "."
                                    }
                                ]
                            }
                        },
                        "hw_search": "a",
                        "def_search": "foo"
                    }
                ]
            }
        "#);

        check!("a|b|c|foo \\this.", r#"
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
                        "primary_definition": {
                            "def": {
                                "group": [
                                    {
                                        "text": "foo "
                                    },
                                    {
                                        "macro": {
                                            "name": "this"
                                        }
                                    },
                                    {
                                        "text": "."
                                    }
                                ]
                            }
                        },
                        "hw_search": "a",
                        "def_search": "foo"
                    }
                ]
            }
        "#);

        check!("a|b|c|foo\\ldots", r#"
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
                        "primary_definition": {
                            "def": {
                                "group": [
                                    {
                                        "text": "foo"
                                    },
                                    {
                                        "macro": {
                                            "name": "ellipsis"
                                        }
                                    }
                                ]
                            }
                        },
                        "hw_search": "a",
                        "def_search": "foo"
                    }
                ]
            }
        "#);

        check!("a\\x{de}|b\\x{0de}|c\\x{00de}|\\x{000000de}", r#"
            {
                "entries": [
                    {
                        "word": {
                            "text": "aÞ"
                        },
                        "pos": {
                            "text": "bÞ"
                        },
                        "etym": {
                            "text": "cÞ"
                        },
                        "primary_definition": {
                            "def": {
                                "text": "Þ."
                            }
                        },
                        "hw_search": "a",
                        "def_search": ""
                    }
                ]
            }
        "#);

        // Trailing newline.
        check!("a|b|c|d\n", r#"
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
                        "primary_definition": {
                            "def": {
                                "text": "d."
                            }
                        },
                        "hw_search": "a",
                        "def_search": "d"
                    }
                ]
            }
        "#);

        // Comments.
        check!("a|b|c|d aefaef #comment\n#comment\n foo#comment\n bar #comment", r#"
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
                        "primary_definition": {
                            "def": {
                                "text": "d aefaef foo bar."
                            }
                        },
                        "hw_search": "a",
                        "def_search": "aefaef bar d foo"
                    }
                ]
            }
        "#);
    }

    #[test]
    fn test_entry_parser_errors() {
        let mut g = Generator::new(Default::default());
        macro_rules! check_err {
            ($in:literal, $out:literal) => {
                assert_contains!(g.parse($in, "<input>").unwrap_err(), $out);
            };
        }

        check_err!(
            "x|y|z|\\comment abcd",
            "\\comment is not allowed in an empty sense or empty primary definition. Use \\i{...} instead."
        );

        check_err!(
            "x|y|z|\\\\\\comment abcd",
            "\\comment is not allowed in an empty sense or empty primary definition. Use \\i{...} instead."
        );

        check_err!(
            "x|y|z|\\ex abcd",
            "\\ex is not allowed in an empty sense or empty primary definition."
        );

        check_err!(
            "x|y|z|\\\\\\ex abcd",
            "\\ex is not allowed in an empty sense or empty primary definition."
        );

        check_err!(
            "\\\\a|||",
            "'\\\\' cannot be used in the lemma"
        );

        check_err!(
            "\\comment|||",
            "'\\comment' cannot be used in the lemma"
        );

        check_err!(
            "\\ex|||",
            "'\\ex' cannot be used in the lemma"
        );

        check_err!(
            "foo",
            "An entry must contain at least one '|' or '>'"
        );

        check_err!(
            "\\comment > b",
            "'\\comment' cannot be used in a reference entry"
        );

        check_err!(
            "a > \\comment",
            "'\\comment' cannot be used in a reference entry"
        );

        check_err!(
            "\\ex > b",
            "'\\ex' cannot be used in a reference entry"
        );

        check_err!(
            "a > \\ex",
            "'\\ex' cannot be used in a reference entry"
        );

        check_err!(
            "\\\\ > b",
            "'\\\\' cannot be used in a reference entry"
        );

        check_err!(
            "a > \\\\",
            "'\\\\' cannot be used in a reference entry"
        );

        check_err!(
            "a|b|c|d \\comment abc \\comment abc",
            "'\\comment' cannot be used in a comment"
        );

        check_err!("a\\comment|b|c|d", "'\\comment' cannot be used in the lemma");
        check_err!("a\\ex|b|c|d", "'\\ex' cannot be used in the lemma");
        check_err!("a\\\\|b|c|d", "'\\\\' cannot be used in the lemma");

        check_err!("a|b\\comment|c|d", "'\\comment' cannot be used in this field");
        check_err!("a|b\\ex|c|d", "'\\ex' cannot be used in this field");
        check_err!("a|b\\\\|c|d", "'\\\\' cannot be used in this field");

        check_err!("a|b|c\\comment|d", "'\\comment' cannot be used in this field");
        check_err!("a|b|c\\ex|d", "'\\ex' cannot be used in this field");
        check_err!("a|b|c\\\\|d", "'\\\\' cannot be used in this field");

        check_err!("a|b|c|d|\\comment", "'\\comment' cannot be used in this field");
        check_err!("a|b|c|d|\\ex", "'\\ex' cannot be used in this field");
        check_err!("a|b|c|d|\\\\", "'\\\\' cannot be used in this field");

        check_err!("a > b \\comment", "'\\comment' cannot be used in a reference entry");
        check_err!("a > b \\ex", "'\\ex' cannot be used in a reference entry");
        check_err!("a > b \\\\", "'\\\\' cannot be used in a reference entry");

        check_err!("a\\comment > b", "'\\comment' cannot be used in a reference entry");
        check_err!("a\\ex > b", "'\\ex' cannot be used in a reference entry");
        check_err!("a\\\\ > b", "'\\\\' cannot be used in a reference entry");

        check_err!("|a|b|c", "Lemma must not be empty");
        check_err!("    |a|b|c", "Lemma must not be empty");
        check_err!("  \t |a|b|c", "Lemma must not be empty");
        check_err!("a|b|c|", "Definition must not be empty");
        check_err!("a|b|c| ", "Definition must not be empty");
        check_err!("a|b|c|    ", "Definition must not be empty");
        check_err!("a|b|c|  \t  ", "Definition must not be empty");
        check_err!("a|b|c|\n", "Definition must not be empty");
        check_err!("a|b|c|  \n  ", "Definition must not be empty");
    }

    #[test]
    fn test_always_generate_ipa() {
        let mut g = Generator::new(Options { always_include_ipa: true, ..Default::default() });
        g.parse("$ipa { s|(.+)|/$1:$1/| }\n", "<input>").unwrap();
        check_with_generator!(g, "a|b|c|d", r#" {
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
                    "ipa": {
                        "text": "/a:a/"
                    },
                    "primary_definition": {
                        "def": {
                            "text": "d."
                        }
                    }
                }
            ]
        }"#);
    }

    // Test that preprocess_full_entry() includes a source location.
    #[test]
    fn test_preprocess_full_entry() {
        let mut g = Generator::new(Default::default());
        g.parse("$preprocess { pos { !m/./ \"foobar\" } } \n", "<input>").unwrap();

        macro_rules! check_err {
            ($in:literal, $out:literal) => {
                assert_contains!(g.parse($in, "<input>").unwrap_err(), $out);
            };
        }

        check_err!("a|b|c|d", "foobar");
    }

    // Test applying replacements.
    #[test]
    fn test_replacements() {
        macro_rules! check {
            ($op:expr, $input:literal, $output:literal) => {
                check!($op, $input, $output, false);
            };
            ($op:expr, $input:literal, $output:literal, $lemma:expr) => {
                let mut s = SmallCow::Borrowed($input);
                Parser::apply_string_replacement_op(&mut s, &$op, $lemma);
                assert_eq!(s.as_str(), $output);
            };
        }

        check!(Lower, "ABCD", "abcd");

        {
            let mut g = Generator::new(Default::default());
            g.parse(r#"$preprocess { etym { s/^(\d+) (.+)$/\psc{$1}{$2}/ } }"#, "<input>").unwrap();
            let PreprocessOp::Replace(op) = &g.preprocessor.first().unwrap().op else { unreachable!() };
            check!(op, "50 foobar", r#"\psc{50}{foobar}"#);
        }

        {
            let mut g = Generator::new(Default::default());
            g.parse("$preprocess { etym { trie (nfd) { f|o => *\n } } }", "<input>").unwrap();
            let PreprocessOp::Replace(op) = &g.preprocessor.first().unwrap().op else { unreachable!() };
            check!(op, "50 foobar", r#"50 bar"#);
        }

        {
            let mut g = Generator::new(Default::default());
            g.parse(r#"$preprocess { etym { s/./\x{1234}/ } }"#, "<input>").unwrap();
            let PreprocessOp::Replace(op) = &g.preprocessor.first().unwrap().op else { unreachable!() };
            check!(op, "abcd", "\u{1234}\u{1234}\u{1234}\u{1234}");
        }

        {
            let mut g = Generator::new(Default::default());
            assert_contains!(
                g.parse("$ipa { trie { [aa] => b } }", "<input>").unwrap_err(),
                "Duplicate pattern 'a' in replacement trie"
            );

            assert_contains!(
                g.parse("$ipa { trie { [a] => b \n a => b } }", "<input>").unwrap_err(),
                "Duplicate pattern 'a' in replacement trie"
            );

            assert_contains!(
                g.parse("$ipa { trie { a => b \n a => b } }", "<input>").unwrap_err(),
                "Duplicate pattern 'a' in replacement trie"
            );

            g.parse("$ipa { trie { [] => * \n [] => * } }", "<input>").unwrap();
        }

        {
            check!(StringReplacementOp::Lemma(vec![Lower]), "ABCD", "abcd", true);
            check!(StringReplacementOp::Lemma(vec![Lower]), "ABCD", "ABCD", false);
        }
    }

    #[test]
    fn test_collate() {
        {
            let mut g = Generator::new(Default::default());
            g.parse("$collate { by \"αβγδεϝζηθικϗλꟜμνξοπρσςτυφχψωȣ\" }", "<input>").unwrap();
        }

        {
            let mut g = Generator::new(Default::default());
            g.parse("$collate { by \"abcd\" }", "<input>").unwrap();
            assert_contains!(
                g.parse("qqqq|q|q|q", "<input>").unwrap_err(),
                "Collation of 'qqqq' resulted in an empty word"
            );
        }

        {
            let mut g = Generator::new(Default::default());
            g.parse("$collate { preprocess { s/.// } }", "<input>").unwrap();
            assert_contains!(
                g.parse("qqqq|q|q|q", "<input>").unwrap_err(),
                "Collation of 'qqqq' resulted in an empty word"
            );
        }
    }

    #[test]
    fn test_dir_errors() {
        let mut g = Generator::new(Default::default());
        g.parse("$ipa { lemma { } }", "<input>").unwrap();
        assert_contains!(
            g.parse("$ipa { lemma { lemma {} } }", "<input>").unwrap_err(),
            "'lemma {}' cannot appear within 'lemma {}'"
        );

        assert_contains!(
            g.parse("$declare abþd 2", "<input>").unwrap_err(),
            "Invalid character 'þ' in macro name"
        );

        assert_contains!(
            g.parse("$declare abþ 2", "<input>").unwrap_err(),
            "Invalid character 'þ' in macro name"
        );
    }

    #[test]
    fn test_caret() {
        let mut g = Generator::new(Default::default());
        macro_rules! check_err {
            ($in:literal, $out:literal) => {
                assert_eq!(g.parse($in, "<input>").unwrap_err(), $out);
            };
        }

        // Test that the caret and source ranges are placed properly.
        check_err!("$ipa {\n  trie {\n    [abcda] => 3 \n }\n }", r"
            Error: Parse Error
               ╭─[ <input>:3:10 ]
               │
             3 │     [abcda] => 3
               │      ┬   ┬
               │      ╰────── Previous instance was here
               │          │
               │          ╰── Duplicate pattern 'a' in replacement trie
            ───╯
        ");

        check_err!("$ipa {\n  trie {\n    a|a => 3 \n }\n }", r"
            Error: Parse Error
               ╭─[ <input>:3:7 ]
               │
             3 │     a|a => 3
               │     ┬ ┬
               │     ╰──── Previous instance was here
               │       │
               │       ╰── Duplicate pattern 'a' in replacement trie
            ───╯
        ");

        check_err!("a|b|c|d\n    \\comment foo\n    \\comment bar", r"
            Error: Parse Error
               ╭─[ <input>:3:5 ]
               │
             3 │     \comment bar
               │     ────┬───
               │         ╰───── '\comment' cannot be used in a comment
            ───╯
        ");
    }
}
