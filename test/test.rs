#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_tex_parser() {
        let mut g = Generator::new(Default::default());

        macro_rules! check {
            ($in:literal, $out:literal) => {
                let p = Parser::new(&mut g, $in);
                assert_eq!(p.parse_tex($in.as_bytes()).unwrap().render(), $out);
            };
        }

        macro_rules! check_err {
            ($in:literal, $out:literal) => {
                let p = Parser::new(&mut g, $in);
                assert_eq!(p.parse_tex($in.as_bytes()).unwrap_err(), $out);
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
        check_err!("{", "Error near line 1: Unexpected end of input. Did you forget a '}}'?");
        check_err!("{{}", "Error near line 1: Unexpected end of input. Did you forget a '}}'?");
        check_err!("}", "Error near line 1: Too many '}'s!");
        check_err!("{}}", "Error near line 1: Too many '}'s!");
        check_err!("{}{", "Error near line 1: Unexpected end of input. Did you forget a '}}'?");
        check_err!("{}{}}", "Error near line 1: Too many '}'s!");

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
        check_err!("\\@", "Error near line 1: Unknown macro '\\@'; did you forget to '$declare @ 0' somewhere?");
        check_err!("\\[", "Error near line 1: Unknown macro '\\['; did you forget to '$declare [ 0' somewhere?");
        check_err!("\\]", "Error near line 1: Unknown macro '\\]'; did you forget to '$declare ] 0' somewhere?");

        // Missing macro name.
        check_err!("\\", "Error near line 1: Invalid macro escape sequence");

        // Special macros aren't normally valid.
        check_err!("\\\\", "Error near line 1: '\\\\' cannot be used in this field");
        check_err!("\\ex", "Error near line 1: '\\ex' cannot be used in this field");
        check_err!("\\comment", "Error near line 1: '\\comment' cannot be used in this field");

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
        check_err!("\\x", "Error near line 1: Expected '{' after '\\x'");
        check_err!("\\x{", "Error near line 1: Missing '}' in Unicode escape sequence");
        check_err!("\\x{}", "Error near line 1: Expected at least 1 digit in '\\x{...}'");
        check_err!("\\x{100000000}", "Error near line 1: Invalid unicode codepoint '100000000'");
        check_err!("\\x{q}", "Error near line 1: Invalid unicode codepoint 'q'");
    }

    #[test]
    fn test_declare_directive() {
        let mut g = Generator::new(Default::default());
        g.parse("$declare foo 0\n$declare bar 1\n").unwrap();

        macro_rules! check {
            ($in:literal, $out:literal) => {
                let p = Parser::new(&mut g, $in);
                let node = p.parse_tex($in.as_bytes()).unwrap().render();
                assert_eq!(
                    json::parse(&node).unwrap().to_string(),
                    json::parse($out).unwrap().to_string()
                );
            };
        }

        macro_rules! check_err {
            ($in:literal, $out:literal) => {
                let p = Parser::new(&mut g, $in);
                let res = p.parse_tex($in.as_bytes());
                assert_eq!(res.unwrap_err().to_string(), $out);
            };
        }

        check!("\\foo", "{\"custom_macro\":{\"name\":\"foo\"}}");
        check!("\\bar{xyz}", "{\"custom_macro\":{\"name\":\"bar\",\"args\":[{\"text\":\"xyz\"}]}}");

        check_err!("\\bar", "Error near line 1: Macro '\\bar' expects 1 argument, but got 0");
        check_err!("\\bar{123}{234}", "Error near line 1: Macro '\\bar' expects 1 argument, but got 2");
        check_err!("\\foo{xyz}", "Error near line 1: Macro '\\foo' expects 0 arguments, but got 1");
        check_err!("\\foo{xyz}{abc}", "Error near line 1: Macro '\\foo' expects 0 arguments, but got 2");
    }

    macro_rules! check_with_generator {
        ($g:expr, $in: literal, $out: literal) => {
            $g.parse($in).unwrap();

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
    }

    #[test]
    fn test_entry_parser_errors() {
        let mut g = Generator::new(Default::default());
        macro_rules! check_err {
            ($in:literal, $out:literal) => {
                assert_eq!(g.parse($in).unwrap_err(), $out);
            };
        }

        check_err!(
            "x|y|z|\\comment abcd",
            "Error near line 1: \\comment is not allowed in an empty sense or empty primary definition. Use \\i{...} instead."
        );

        check_err!(
            "x|y|z|\\\\\\comment abcd",
            "Error near line 1: \\comment is not allowed in an empty sense or empty primary definition. Use \\i{...} instead."
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

        check_err!(
            "a|b|c|d \\comment abc \\comment abc",
            "Error near line 1: '\\comment' cannot be used in a comment"
        );

        check_err!("a\\comment|b|c|d", "Error near line 1: '\\comment' cannot be used in the lemma");
        check_err!("a\\ex|b|c|d", "Error near line 1: '\\ex' cannot be used in the lemma");
        check_err!("a\\\\|b|c|d", "Error near line 1: '\\\\' cannot be used in the lemma");

        check_err!("a|b\\comment|c|d", "Error near line 1: '\\comment' cannot be used in this field");
        check_err!("a|b\\ex|c|d", "Error near line 1: '\\ex' cannot be used in this field");
        check_err!("a|b\\\\|c|d", "Error near line 1: '\\\\' cannot be used in this field");

        check_err!("a|b|c\\comment|d", "Error near line 1: '\\comment' cannot be used in this field");
        check_err!("a|b|c\\ex|d", "Error near line 1: '\\ex' cannot be used in this field");
        check_err!("a|b|c\\\\|d", "Error near line 1: '\\\\' cannot be used in this field");

        check_err!("a|b|c|d|\\comment", "Error near line 1: '\\comment' cannot be used in this field");
        check_err!("a|b|c|d|\\ex", "Error near line 1: '\\ex' cannot be used in this field");
        check_err!("a|b|c|d|\\\\", "Error near line 1: '\\\\' cannot be used in this field");

        check_err!("a > b \\comment", "Error near line 1: '\\comment' cannot be used in a reference entry");
        check_err!("a > b \\ex", "Error near line 1: '\\ex' cannot be used in a reference entry");
        check_err!("a > b \\\\", "Error near line 1: '\\\\' cannot be used in a reference entry");

        check_err!("a\\comment > b", "Error near line 1: '\\comment' cannot be used in a reference entry");
        check_err!("a\\ex > b", "Error near line 1: '\\ex' cannot be used in a reference entry");
        check_err!("a\\\\ > b", "Error near line 1: '\\\\' cannot be used in a reference entry");

        check_err!("|a|b|c", "Error near line 1: Lemma must not be empty");
        check_err!("    |a|b|c", "Error near line 1: Lemma must not be empty");
        check_err!("  \t |a|b|c", "Error near line 1: Lemma must not be empty");
        check_err!("a|b|c|", "Error near line 1: Definition must not be empty");
        check_err!("a|b|c| ", "Error near line 1: Definition must not be empty");
        check_err!("a|b|c|    ", "Error near line 1: Definition must not be empty");
        check_err!("a|b|c|  \t  ", "Error near line 1: Definition must not be empty");
        check_err!("a|b|c|\n", "Error near line 1: Definition must not be empty");
        check_err!("a|b|c|  \n  ", "Error near line 1: Definition must not be empty");
    }

    #[test]
    fn test_always_generate_ipa() {
        let mut g = Generator::new(Options { always_include_ipa: true, ..Default::default() });
        g.parse("$ipa { s|(.+)|/$1:$1/| }\n").unwrap();
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
        g.parse("$preprocess { pos { !m/./ \"foobar\" } } \n").unwrap();

        macro_rules! check_err {
            ($in:literal, $out:literal) => {
                assert_eq!(g.parse($in).unwrap_err(), $out);
            };
        }

        check_err!("a|b|c|d", "Error near line 1: foobar");
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
            g.parse(r#"$preprocess { etym { s/^(\d+) (.+)$/\psc{$1}{$2}/ } }"#).unwrap();
            let PreprocessOp::Replace(op) = &g.preprocessor.first().unwrap().op else { unreachable!() };
            check!(op, "50 foobar", r#"\psc{50}{foobar}"#);
        }

        {
            let mut g = Generator::new(Default::default());
            g.parse("$preprocess { etym { trie (nfd) { f|o => *\n } } }").unwrap();
            let PreprocessOp::Replace(op) = &g.preprocessor.first().unwrap().op else { unreachable!() };
            check!(op, "50 foobar", r#"50 bar"#);
        }

        {
            let mut g = Generator::new(Default::default());
            g.parse(r#"$preprocess { etym { s/./\x{1234}/ } }"#).unwrap();
            let PreprocessOp::Replace(op) = &g.preprocessor.first().unwrap().op else { unreachable!() };
            check!(op, "abcd", "\u{1234}\u{1234}\u{1234}\u{1234}");
        }

        {
            let mut g = Generator::new(Default::default());
            assert_eq!(
                g.parse("$ipa { trie { [aa] => b } }").unwrap_err(),
                "Error near line 1: Duplicate pattern 'a' in replacement trie"
            );

            assert_eq!(
                g.parse("$ipa { trie { [a] => b \n a => b } }").unwrap_err(),
                "Error near line 1: Duplicate pattern 'a' in replacement trie"
            );

            assert_eq!(
                g.parse("$ipa { trie { a => b \n a => b } }").unwrap_err(),
                "Error near line 1: Duplicate pattern 'a' in replacement trie"
            );

            g.parse("$ipa { trie { [] => * \n [] => * } }").unwrap();
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
            g.parse("$collate { by \"αβγδεϝζηθικϗλꟜμνξοπρσςτυφχψωȣ\" }").unwrap();
        }

        {
            let mut g = Generator::new(Default::default());
            g.parse("$collate { by \"abcd\" }").unwrap();
            assert_eq!(
                g.parse("qqqq|q|q|q").unwrap_err(),
                "Error near line 1: Collation of 'qqqq' resulted in an empty word"
            );
        }

        {
            let mut g = Generator::new(Default::default());
            g.parse("$collate { preprocess { s/.// } }").unwrap();
            assert_eq!(
                g.parse("qqqq|q|q|q").unwrap_err(),
                "Error near line 1: Collation of 'qqqq' resulted in an empty word"
            );
        }
    }

    #[test]
    fn test_ipa_dir_error() {
        let mut g = Generator::new(Default::default());
        g.parse("$ipa { lemma { } }").unwrap();
        assert_eq!(g.parse("$ipa { lemma { lemma {} } }").unwrap_err(), "Error near line 1: 'lemma {}' cannot appear within 'lemma {}'");
    }
}
