app [main] {
    pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.15.0/SlwdbJ-3GR7uBWQo6zlmYWNYOxnvo8r6YABXD-45UOw.tar.br",
    rvn: "../package/main.roc",
}

import pf.Stdout
import rvn.Rvn

str =
    """
    {
        language: "Roc",
        tags: ["Fast", "Friendly", "Functional"],
        ignored: "This property will be ignored",
        color: 0x7c38f5, # supports hex digits, comments too!
    }
    """

Example : {
    language : Str,
    tags : List Str,
    color : U32,
}

main =
    decoder = Rvn.compact
    decoded : DecodeResult Example
    decoded =
        str
        |> Str.toUtf8
        |> Decode.fromBytesPartial decoder
    Stdout.line "$(Inspect.toStr decoded)"
