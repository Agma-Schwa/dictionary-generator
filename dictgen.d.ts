export namespace Dictionary {
    export type Data = { entries: Entry[] }
    export type Entry = FullEntry | RefEntry

    export type RefEntry = {
        word: Node,
        refs: Node[],
        search?: string,
    }

    export type FullEntry = {
        word: Node,
        pos: Node,
        etym: Node,
        ipa?: Node,
        primary_definition?: Sense,
        senses?: Sense[],
        forms?: Node,
        hw_search?: string,
        def_search?: string,
    }

    export type Sense = {
        def: Node,
        comment?: Node,
        examples?: Example[],
    }

    export type Example = {
        text: Node,
        comment?: Node
    }

    export type Node = TextNode
                     | MathNode
                     | GroupNode
                     | MacroNode

    export type TextNode = { text: string }
    export type MathNode = { math: string }
    export type GroupNode = { group: Node[] }
    export type MacroNode = {
        name: BuiltinMacro,
        args?: Node[]
    }

    export type BuiltinMacro = "bold"
                             | "ellipsis"
                             | "italic"
                             | "lemma"
                             | "normal"
                             | "paragraph_break"
                             | "sense"
                             | "small_caps"
                             | "subscript"
                             | "superscript"
                             | "soft_hyphen"
                             | "this"
}
