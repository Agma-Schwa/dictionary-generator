# Dictionary Generator
This program takes a dictionary written in a custom file format and converts it to JSON; the file format is intended to make writing dictionary entries as easy and terse as possible without compromising flexibility.

## Building and Using the Generator
This is a Rust project, so [install Rust](https://doc.rust-lang.org/book/ch01-01-installation.html) on your system and build it using cargo:
```bash
$ cargo build --release
```
This creates a `generator` binary in `target/release/generator`. You can then invoke the program by passing it a file and it will print the generated dictionary to stdout:
```bash
$ ./target/release/generator my_dictionary.dict.txt
```
In addition to a standalone executable, the generator proper is a standalone crate that can be consumed as a library; the implementation of the `generator` binary in [src/main.rs](src/main.rs) serves as a good example that demonstrates the API of the library.

Lastly, this repository also bundles a WASM plugin for use with Typst. This enables adding dictionaries to a Typst project witout requiring a separate build step. Our Typst [template library](https://github.com/Agma-Schwa/typst-base) that we use for all of our project includes bindings for it. Just search for `instantiate-dictionary-plugin`.

If you don't want to depend on our library, you can build the plugin yourself; simply run:
```bash
$ cargo build --release --target wasm32-unknown-unknown --lib
```
The compiled plugin can then be found in `target/wasm32-unknown-unknown/release/dictgen.wasm`. The functions that are exposed by the plugin can be found in [src/wasm.rs](src/wasm.rs).

## Editor Tools
If you’re using vscode, an extension that provides syntax highlighting can be found in [vscode-plugin](vscode-plugin); you can build it by running `vsce package`; this creates a `dictionary-file-format-<version>.vsix` file; you can right-click on that in vscode and then select ‘Install Extension VSIX’ at the bottom of the drop-down to install it.

## File Format
The following is an exhaustive description of the dictionary file format; a dictionary file consists of 2 sections:
1. an optional _preamble_ containing _directives_ which define text-to-text transformations for e.g. converting text in the source language to IPA;
2. the actual dictionary _entries_; these can be in any order as the generator will sort them alphabetically.

In either part, any text in a line after a `#` is _ignored_ in most circumstances.

## Preamble
The preamble consists of any number of the directives described below; all directives start with a `$` sign which _must_ be the very first character in a line (including whitespace).

### `$ipa` Directive
The `$ipa` directive defines a set of transformations for converting text in the language described by the dictionary to IPA. The syntax of the directive is:
```
$ipa {
    <transformations>
}
```
All supported transformations are described in the ‘String Replacement Ops’ section below.

### `$preprocess` Directive
The `$preprocess` directive runs string-to-string transformations on the _parts_ of a _full entry_ (see the ‘Full Entry’ section below) before they are parsed. The syntax of this directive is:
```
$preprocess {
    <part> { <transformations> }
    # ...
}
```
Here `<part>` must be one of:
- `pos` (part of speech),
- `etym` (etymology),
- `def` (definition),
- `forms`,
- `ipa`.

The `<transformations>` are then applied to the contents of the `<part>`; a single `$preprocess`
directive may contain multiple parts. All supported transformations are described in the ‘String Replacement Ops’ section below.

### `$collate` Directive
The `$collate` directive can be used to alter the _collation_, i.e. sort order, of dictionary entries. This directive is of the form
```
$collate {
    by "..."
    preprocess {
        <transformations>
    }
}
```
Both the `by` and `preprocess` clauses are optional; the contents of the `<preprocess>` clause
are string transformations; all supported transformations are described in the ‘String Replacement Ops’ section below. The `by` clause consists of a list of characters enclosed in `""`.

The effect of this clause is as follows: if a `preprocess` clause is present, the transformations therein are applied to each lemma. If a `by` clause is present, every character in the lemma that does _not_ appear in the `by` clause is deleted, and the sort order of the words is based of the order of characters in the `by` clause.

For example, the following collation
```
$collate {
    by "tmoae"
    preprocess { lower }
}
```
has the following effect: First, the lemma is converted to lowercase; then, all characters in it that are not `t`, `m`, `o`, `a`, or `e` are deleted. Finally, words are sorted according to that order; e.g. if we have the 3 entries `ta`, `mo`, `to` in the dictionary, they will be sorted as `to`, `ta`, `mo`.

Note that the `preprocess` directive is _only_ used for sorting. It does not affect the final lemmas that end up in the output. This means you can use it to simplify the words quite a bit by deleting ‘unnecessary’ parts that aren’t needed for sorting.

The generator will emit an error if a collation ends up deleting _all_ characters in a word. If you don’t specify a collation, the _default collation_ is:
```
$collate {
    preprocess {
        nfkd
        remove_punct
        nfc
        lower
    }
}
```

### `$declare` Directive
The `$declare` directive declares a custom macro; to combat typos, the generator errors if it encounters a macro it doesn’t recognise (see the ‘Macros’ section below). This directive provides a way to tell a generator that a certain macro should be accepted. Its syntax is
```
$declare <name> <args>
```
where `<name>` is the name of a macro _without_ the `\`, and `<args>` is a non-negative integer that describes how many arguments the macro takes; e.g.
```
$declare foo 2
```
declares a macro `\foo` that takes `2` arguments. That is, the generator will then accept e.g. `\foo{bar}{baz}`, but _not_ `\foo`, `\foo{bar}`, or `\foo{bar}{baz}{quux}`.

Note that this _does not_ define what the macro actually does! The generator will emit a `custom_macro` node that you will then have to handle wherever you actually make use of the JSON output. The API of our Typst library allows you to define a `custom-macro-handler` for this purpose.

## String Replacement Ops
This section describes the string-to-string transformations that can be used in various parts of the preamble. Wherever `<transformations>` appears above, you can insert any number of these transformations; they will be applied in order of appearance.

All transformations are Unicode-aware.

### `lemma`
This transformation is _only_ valid within an `$ipa` directive and has the following syntax:
```
lemma {
    <transformations>
}
```
Its function is to specify transformations that are _only_ applied if we’re converting a lemma in the dictionary to IPA. They are _not_ applied whenever standalone IPA conversion is requested, e.g. via the generator’s `--convert` flag, by calling `Generator::to_ipa()`, or by using the Typst library’s `to_ipa()` function.

This can be useful if you want to e.g. drop everything after a comma when converting a lemma to IPA, but you also want to be able to pass an entire text (containing commas) to the generator to convert it to IPA w/o it stripping everything after the first comma.

`lemma` directives cannot be nestead as that would not be useful.

### `lower`
This transformation has the following syntax:
```
lower
```
It converts the input string to lowercase.

### `nfc`, `nfd`, `nfkc`, `nfkd`
These transformations have the following syntax:
```
nfc
```
Or instead of `nfc`, you can write `nfd`, `nfkc`, or `nfkd`. These transformations convert the input into the corresponding [Unicode normalisation form](https://www.unicode.org/reports/tr15/#Norm_Forms).

### `m`
This transformation is _only_ valid within the `$preprocess` directive and has the following syntax:
```
 m/<regex>/ "message"
!m/<regex>/ "message" # negated version
```
This is not really a transformation; rather it matches the input against the _regular expression_ `<regex>`; if it _doesn’t_ match, an error containing `"message"` is printed. The `m` may be preceded by `!`, which negates the match, i.e. the error is emitted if the input _does_ match.

This can be used to verify that the input is well-formed. For example, given
```
$preprocess {
    etym { !m/b/ "We don’t allow 'b's here!" }
}
```
the generator will print the error `"We don’t allow 'b's here!"` if the etymology field of a full entry contains a `b` character.

The syntax used is that of Rust’s `fancy-regex` crate, which essentially supports Oniguruma regular expressions. If you’re not familiar with regexes, I suggest starting with the [Wikipedia page](https://en.wikipedia.org/wiki/Regular_expression).

### `remove_punct`
This transformation has the following syntax:
```
remove_punct
```
It deletes ‘punctuation’ from the input (including diacritics); specifically, this removes any characters in any of the following Unicode categories: `Mc`, `Me`, `Mn`, `Pc`, `Pd`, `Pe`, `Pf`, `Pi`, `Po`, `Ps`.

### `s`
This transformation has the following syntax:
```
s/<regex>/<replacement>/
```
Instead of `/`, you can also use `|` or `%`, but you can’t mix them, e.g. `s|a|b|` is vaid, but `s/a/b|` is not.

This transformation replaces every occurrence of the _regular expression_ `<regex>` with `<replacement>`. The syntax used is that of Rust’s `fancy-regex` crate, which essentially supports Oniguruma regular expressions. If you’re not familiar with regexes, I suggest starting with the [Wikipedia page](https://en.wikipedia.org/wiki/Regular_expression).

### `trie`
This transformation is quite a bit more complicated and has the following syntax:
```
trie (<norm>) {
    <pattern> => <replacement>
    <pattern> => <replacement>
    # ... more patterns
}
```
As for the individual components:
-  `<norm>` is a [Unicode normalisation form](https://www.unicode.org/reports/tr15/#Norm_Forms), i.e. either `nfc`, `nfd`, `nfkc`, or `nfkd`. You can also omit this entirely, in which case no normalisation is performed.
- `<replacement>` is a sequence of characters; if it is a single `*`, then the pattern is _deleted_.
- `<pattern>` is _not_ a regular expression, even though its syntax is similar to one: it is either
    - a set of characters enclosed in square brackets, e.g. `[abc]`; sets may contain whitespace, e.g. `[ ]`; if you want to include `\` or `]` in a set, write `\\` or `\]` instead, e.g. `[\]]`; or
    - a sequence of characters, e.g. `abc`; you can also combine multiple sequences in one line using `|`, e.g. `abc|def => x` is equivalent to writing both `abc => x` and `def => x`.

    You _cannot_ combine a sequence and a set at the moment; this may be supported in the future.

In both the `<replacement>` and `<pattern>`, you can specify any Unicode character by writing `\x{<code>}` where `<code>` is a hexadecimal Unicode code point, e.g. `\x{00DE}` would be `Þ`.

This clause performs text replacement: First, `<norm>` is applied to _both_ the input text _and_ all `<pattern>`s. Then, each `<pattern>` in the input text is replaced with its `<replacement>`. All patterns are matched and replaced _simultaneously_ in a single pass:
- If `<pattern>` is a set, e.g. `[abc]`, each individual character in the set is replaced; e.g. if we have `[abc] => foo`, every occurrence of `a`, `b`, or `c` in the input is replaced with `foo`.
- If pattern is a sequence, e.g. `abc`, that entire sequence is replaced. If multiple sequences would match, the _longest_ one is replaced, e.g. if you specify `ab => x` and `abc => y`, any `abc` sequence in the input will be replaced with `y`.
- Because this happens in a single pass, there is no iterative replacement, e.g. if you write `a => b` and `b => c`, then an input of `ab` would become `bc`, _not_ `cc`.

## Entries
There are 2 types of entries: _full entries_ and _reference entries_. By default, each non-empty line in a dictionary file (that is not part of a directive) starts a new entry. To break an entry across multiple lines, indent the continuation lines with at least one a space or tab.

Line-breaking does not affect how the text is displayed. Moreover, _any_ sequence of whitespace characters is replaced with a single space, and whitespace at the start and end of a field in a full entry or word in a reference entry is removed.

As in the preamble, anything after the first `#` in a line is ignored. Empty lines are ignored as well; in particular, you can have an empty line between two continuation lines without starting a new entry.

### Full Entry
A full entry consists of multiple _fields_, separated by `|`. Thus, fields cannot _contain_ a `|` character. The fields are, in order:

1. The lemma.
2. The part of speech and other initial annotations.
3. The etymology of the word.
4. The definition of the word. This may consist of a primary sense and several additional senses (see below).
5. _(optional)_ Grammatical forms of the word.
6. _(optional)_ The pronunciation of the word in IPA. Only provide this if it is irregular; if the IPA can be derived from the spelling, prefer to use an `$ipa` directive instead. If this field is provided, it overrides the `$ipa` directive for this entry.

Any field other than the lemma and definition may be empty. Thus, e.g. `a|||b` would be a valid entry (unless a particular dictionary chooses to disallow this). By contrast e.g. `a||b` is _not_ a valid entry since the definition is missing, and neither is `a||b|`.

#### Definition
The definition field is the most complex: It may contain multiple _senses_; use `\\` to start a new sense. The _primary definition_ is everything before the first sense. A sense, as well as the primary definition, may have a _single_ `\comment` as well as any number of _examples_ that start with `\ex`; each example may also have a _single_ `\comment`:
```
\\ sense 1
    \comment foo
    \ex example 1
         \comment comment for example 1
    \ex example 2
         \comment comment for example 2
```
The indentation depth and line breaks here are optional and only for visual clarity, i.e. this would be equivalent:
```
\\sense 1\comment foo\ex example 1\comment comment for example 1\ex example 2\comment comment for example 2
```

The generator will insert a full stop `.` at the end of every sense (including the primary definition), comment, and example, provided that it doesn’t already end with a punctuation mark. Thus, e.g. `\comment foo.` is equivalent to writing `\comment foo`

**NOTE:** `\\`, `\comment`, and `\ex` are allowed in this field _only_. Crucially, they do _not_ take an argument, i.e. write `\comment This is a comment` rather than `\comment{This is a comment}`.

#### Macros
Macros are used to add markup to an entry. For historical reasons, the syntax of macros is heavily inspired by that of LaTeX macros: a macro name consists of a backslash followed immediately by either one or more letters (e.g. `\Sup`) or a a single special character (e.g. `\$`). Macros are case-sensitive. A macro may also take _arguments_ that follow the macro name and are wrapped in braces `{}`. E.g. the `\s` macro takes a single argument (e.g. `\s{acc}`) and formats that argument in small-caps. Braces anywhere else in the input must be balanced but are otherwise _ignored_.

The following is an exhaustive list of builtin macros:
- `\-`: a soft hyphen; this indicates to the typesetting engine that it _may_ break the word here.
- `\ `: a literal space; by default, multiple spaces in a row are collapsed to a single space.
- `\&`: the character `&`.
- `\$`: the character `$`. Without the `\`, a `$` instead starts math mode.
- `\%`: the character `%`.
- `\%`: the character `%`. You can also just write `%` on its own. It used to have special meaning but doesn’t anymore.
- `\#`: the character `#`. Without the `\`, a `#` instead causes the rest of the line to be ignored.
- `\{`: the character `{`. On its own, a `{` either starts a macro argument or is ignored when not preceded by a macro or another argument.
- `\}`: the character `}`. On its own, this either ends a macro argument or is ignored.
- `\\`: start a new sense; only valid within the definition field; see ‘Definition’ above.
- `\b{arg}`: typeset `arg` in bold.
- `\i{arg}`: typeset `arg` in italics.
- `\comment`: start a comment; only valid within the definition field; see ‘Definition’ above.
- `\ex`: start an example; only valid within the definition field; see ‘Definition’ above.
- `\ldots`: insert an ellipsis: `…`.
- `\par`: insert a paragraph break; only use this if an entry is really long.
- `\ref{label}`: insert a reference to `label`; e.g. to reference a Typst label `<foo>`, write `\ref{foo}`.
- `\s{arg}`: typeset `arg` in small-caps.
- `\senseref{n}`: reference the n-th sense of this entry; prefer e.g. ‘see `\sensref{1}`’ over writing ‘see sense 1’.
- `\Sub{arg}`: typeset `arg` in subscript.
- `\Sup{arg}`: typeset `arg` in superscript.
- `\textnf{arg}`: typeset `arg` as ‘normal’ text, i.e. _not_ bold/italic/small-caps.
- `\this`: insert the lemma of this entry.
- `\w{arg}`: typeset `arg` in the same way as a lemma; use this to refer to other lemmas or words in an entry.
- `\x{codepoint}`: insert the Unicode character with value `codepoint`, e.g. `\x{00DE}` inserts a `Þ`.

Macros can generally be nested, but whether this makes sense depends on the macro, e.g. `\b{\i{arg}}` typesets `arg` in bold italic, whereas `\b{\textnf{arg}}` is a bit pointless, as the `\textnf` just undoes the effect of the `\b`, and `\x{\b{1234}}` is simply an error as `\b{1234}` is not a valid Unicode codepoint.

An unknown macro causes an error unless you `$declare`d it in the preamble (see ‘`$declare` Directive’ above).

#### Examples
An entry with the fields highlighted:
```
dír|v. tr.|dire|+\s{acc} To say, tell (+\s{dat} someone) |\s{fut} dírẹ́, \s{subj} díss
^^^ ^^^^^^ ^^^^ ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ ^^^^^^^^^^^^^^^^^^^^^^^^^^^
1   2      3    4                                         5
```

An entry with 3 senses and no primary definition, as well as a forms field at the very end:
```
syl|v.|seul|
    \\ To be the only one.
    \\ \i{(of multiple things)} To be one, a united whole.
    \\ To be lone, alone|\s{fut} syle, \s{subj} syls
```

This entry has the primary definition of ‘fish’, a comment on the primary
definition, as well as two examples, each with their own comment:
```
ráhó|n.|poisson|Fish
    \comment UF has a series of proverbs around fish drowning (in water!),
        despite the fact that fish quite literally breathe water and therefore
        are incapable of ‘drowning’.
    \ex \w{Láráhó slẹlúrá.} Now you’ve done it.
        \comment Literally ‘the fish was too bulky [to swim to the surface,
             so it drowned]’
    \ex \w{Áhaúr’sý’ýâ láráhó sráy’éá.} There is more to this. There is a
             story behind this.
        \comment Literally ‘the fish hasn’t drowned yet’.
```

### Reference Entry
A reference entry consists of a number of comma-separated words, followed by `>` and another word; e.g.
```
foo, bar > baz
```
creates one entry for `foo` and `bar` each, both of which point to `baz`. This is useful for pointing irregular forms towards the base entry that describes them, e.g. in an English dictionary, one might write:
```
are, is, was, were, been > be
```
The character `|` cannot be used in a reference entry.
