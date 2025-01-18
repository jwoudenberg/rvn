# This module is vendored from here:
# https://github.com/jwoudenberg/rvn/blob/main/package/Rvn.roc
#
# I haven't been able to pull it in as a package dependency (yet).
#
#
#
#
#
#
## RVN is a serialization format like JSON, YAML, or XML, but made to look
## like Roc code. This means you can use records, lists, and even tags in your
## serialized data.
##
## This is an example of encoding the list `[1,2]` to compact RVN:
##
##     expect
##         actual = Encode.to_bytes [1,2] Rvn.compact
##         expected = ['[', '1', ',', '2', ',', ']' ]
##         actual == expected
##
##
## This is an example of encoding the list `[1,2]` to prettily-formatted RVN:
##
##     expect
##         actual = Encode.to_bytes [1,2] Rvn.pretty
##         expected = Str.to_utf8
##             """
##             [
##                 1,
##                 2,
##             ]
##             """
##         actual == expected
##
##
## This is an example of decoding some RVN into a Roc value:
##
##     expect
##         actual = Decode.from_bytes ['2', '3'] Rvn.compact
##         expected = Ok 23
##         actual == expected
module [
    compact,
    pretty,
    Rvn,
]

## A type with the `EncoderFormatting` and `DecoderFormatting` abilities.
## You likely don't need this!
Rvn := { indent : U64, format : [Compact, Pretty], in_tag : Bool }
    implements [
        EncoderFormatting {
            u8: encode_u8,
            u16: encode_u16,
            u32: encode_u32,
            u64: encode_u64,
            u128: encode_u128,
            i8: encode_i8,
            i16: encode_i16,
            i32: encode_i32,
            i64: encode_i64,
            i128: encode_i128,
            f32: encode_f32,
            f64: encode_f64,
            dec: encode_dec,
            bool: encode_bool,
            string: encode_string,
            list: encode_list,
            record: encode_record,
            tuple: encode_tuple,
            tag: encode_tag,
        },
        DecoderFormatting {
            u8: decode_u8,
            u16: decode_u16,
            u32: decode_u32,
            u64: decode_u64,
            u128: decode_u128,
            i8: decode_i8,
            i16: decode_i16,
            i32: decode_i32,
            i64: decode_i64,
            i128: decode_i128,
            f32: decode_f32,
            f64: decode_f64,
            dec: decode_dec,
            bool: decode_bool,
            string: decode_string,
            list: decode_list,
            record: decode_record,
            tuple: decode_tuple,
        },
    ]

## Use for encoding Roc values to RVN, or decoding RVN bytes into Roc values.
##
## The encoded RVN will be formatted with minimal whitespace.
compact : Rvn
compact = @Rvn { format: Compact, indent: 0, in_tag: Bool.false }

## Use for encoding Roc values to RVN, or decoding RVN bytes into Roc values.
##
## The encoded RVN will be formatted similarly to the output of `roc format`.
pretty : Rvn
pretty = @Rvn { format: Pretty, indent: 0, in_tag: Bool.false }

expect
    actual = Encode.to_bytes [1, 2] Rvn.pretty
    expected = Str.to_utf8
        """
        [
            1,
            2,
        ]
        """
    actual == expected

num_to_bytes = |n|
    n |> Num.to_str |> Str.to_utf8

encode_u8 = |n|
    Encode.custom |bytes, @Rvn _|
        List.concat bytes (num_to_bytes n)

encode_u16 = |n|
    Encode.custom |bytes, @Rvn _|
        List.concat bytes (num_to_bytes n)

encode_u32 = |n|
    Encode.custom |bytes, @Rvn _|
        List.concat bytes (num_to_bytes n)

encode_u64 = |n|
    Encode.custom |bytes, @Rvn _|
        List.concat bytes (num_to_bytes n)

encode_u128 = |n|
    Encode.custom |bytes, @Rvn _|
        List.concat bytes (num_to_bytes n)

encode_i8 = |n|
    Encode.custom |bytes, @Rvn _|
        List.concat bytes (num_to_bytes n)

encode_i16 = |n|
    Encode.custom |bytes, @Rvn _|
        List.concat bytes (num_to_bytes n)

encode_i32 = |n|
    Encode.custom |bytes, @Rvn _|
        List.concat bytes (num_to_bytes n)

encode_i64 = |n|
    Encode.custom |bytes, @Rvn _|
        List.concat bytes (num_to_bytes n)

encode_i128 = |n|
    Encode.custom |bytes, @Rvn _|
        List.concat bytes (num_to_bytes n)

encode_f32 = |n|
    Encode.custom |bytes, @Rvn _|
        List.concat bytes (num_to_bytes n)

encode_f64 = |n|
    Encode.custom |bytes, @Rvn _|
        List.concat bytes (num_to_bytes n)

encode_dec = |n|
    Encode.custom |bytes, @Rvn _|
        List.concat bytes (num_to_bytes n)

encode_bool = |byte|
    Encode.custom |bytes, @Rvn _|
        if byte then
            List.concat bytes (Str.to_utf8 "Bool.true")
        else
            List.concat bytes (Str.to_utf8 "Bool.false")

expect
    # encode Bool.true
    actual = Encode.to_bytes Bool.true compact
    expected = ['B', 'o', 'o', 'l', '.', 't', 'r', 'u', 'e']
    actual == expected

expect
    # encode Bool.false
    actual = Encode.to_bytes Bool.false compact
    expected = ['B', 'o', 'o', 'l', '.', 'f', 'a', 'l', 's', 'e']
    actual == expected

encode_string = |str|
    Encode.custom |bytes, @Rvn _|
        str_bytes = Str.to_utf8 str

        encode_slice_without_escaping = |{ start, len, acc }| {
            start: start + len,
            len: 0,
            acc: List.concat acc (List.sublist str_bytes { start, len }),
        }

        escape = |state, char|
            { start, acc } = encode_slice_without_escaping state

            {
                start: start + 1,
                len: 0,
                acc: List.concat acc ['\\', char],
            }

        escape_and_append = |state, byte|
            when byte is
                '\n' -> escape state 'n'
                '\t' -> escape state 't'
                '"' -> escape state '"'
                '\\' -> escape state '\\'
                '$' -> escape state '$'
                _ -> { start: state.start, len: state.len + 1, acc: state.acc }

        encode_and_append = |acc|
            List.walk
                str_bytes
                { start: 0, len: 0, acc }
                escape_and_append
            |> encode_slice_without_escaping
            |> .acc

        bytes
        |> List.concat ['"']
        |> encode_and_append
        |> List.concat ['"']

expect
    str = "abc"
    actual = Encode.to_bytes str compact
    expected = ['"', 'a', 'b', 'c', '"']
    actual == expected

expect
    str = "a\nc"
    actual = Encode.to_bytes str compact
    expected = ['"', 'a', '\\', 'n', 'c', '"']
    actual == expected

expect
    str = "a\tc"
    actual = Encode.to_bytes str compact
    expected = ['"', 'a', '\\', 't', 'c', '"']
    actual == expected

expect
    str = "a\"c"
    actual = Encode.to_bytes str compact
    expected = ['"', 'a', '\\', '"', 'c', '"']
    actual == expected

expect
    str = "a\\c"
    actual = Encode.to_bytes str compact
    expected = ['"', 'a', '\\', '\\', 'c', '"']
    actual == expected

expect
    str = "a\$c"
    actual = Encode.to_bytes str compact
    expected = ['"', 'a', '\\', '$', 'c', '"']
    actual == expected

encode_list : List elem, (elem -> Encoder Rvn) -> Encoder Rvn
encode_list = |list, encode_elem|
    Encode.custom |bytes, fmt|
        add_encoded_elem = |acc, elem|
            indented = up_indent fmt
            acc
            |> append_indent indented
            |> Encode.append_with (encode_elem elem) (set_in_tag indented Bool.false)
            |> List.append ','
            |> append_if_pretty indented '\n'

        bytes
        |> List.concat ['[']
        |> append_if_pretty fmt '\n'
        |> |new_bytes| List.walk list new_bytes add_encoded_elem
        |> append_indent fmt
        |> List.concat [']']

expect
    # Encoding an empty list
    list : List U16
    list = []
    actual = Encode.to_bytes list compact
    expected = Str.to_utf8 "[]"
    actual == expected

expect
    # Compact list encoding
    list = [1, 2, 3]
    actual = Encode.to_bytes list compact
    expected = Str.to_utf8 "[1,2,3,]"
    actual == expected

expect
    # Pretty list encoding
    list = [1, 2, 3]
    actual = Encode.to_bytes list pretty
    expected = Str.to_utf8
        """
        [
            1,
            2,
            3,
        ]
        """
    actual == expected

encode_record : List { key : Str, value : Encoder Rvn } -> Encoder Rvn
encode_record = |fields|
    Encode.custom |bytes, fmt|
        add_encoded_field = |acc, { key, value }|
            indented = up_indent fmt
            acc
            |> append_indent indented
            |> List.concat (Str.to_utf8 key)
            |> List.concat [':']
            |> append_if_pretty indented ' '
            |> Encode.append_with value (set_in_tag indented Bool.false)
            |> List.concat [',']
            |> append_if_pretty indented '\n'

        bytes
        |> List.concat ['{']
        |> append_if_pretty fmt '\n'
        |> |new_bytes| List.walk fields new_bytes add_encoded_field
        |> append_indent fmt
        |> List.concat ['}']

expect
    # Encoding an empty record
    record = {}
    actual = Encode.to_bytes record compact
    expected = Str.to_utf8 "{}"
    actual == expected

expect
    # Compact record encoding
    record = { one: 1, two: 2 }
    actual = Encode.to_bytes record compact
    expected = Str.to_utf8 "{one:1,two:2,}"
    actual == expected

expect
    # Pretty record encoding
    record = { one: 1, two: 2 }
    actual = Encode.to_bytes record pretty
    expected = Str.to_utf8
        """
        {
            one: 1,
            two: 2,
        }
        """
    actual == expected

encode_tuple : List (Encoder Rvn) -> Encoder Rvn
encode_tuple = |elems|
    Encode.custom |bytes, fmt|
        add_encoded_elem = |acc, elem|
            indented = up_indent fmt
            acc
            |> append_indent indented
            |> Encode.append_with elem (set_in_tag indented Bool.false)
            |> List.concat [',']
            |> append_if_pretty indented '\n'

        bytes
        |> List.concat ['(']
        |> append_if_pretty fmt '\n'
        |> |new_bytes| List.walk elems new_bytes add_encoded_elem
        |> append_indent fmt
        |> List.concat [')']

expect
    # Compact tuple encoding
    tuple = (1, 2)
    actual = Encode.to_bytes tuple compact
    expected = Str.to_utf8 "(1,2,)"
    actual == expected

expect
    # Pretty tuple encoding
    tuple = (1, 2)
    actual = Encode.to_bytes tuple pretty
    expected = Str.to_utf8
        """
        (
            1,
            2,
        )
        """
    actual == expected

encode_tag : Str, List (Encoder Rvn) -> Encoder Rvn
encode_tag = |tag, attrs|
    Encode.custom |bytes, fmt|
        (@Rvn { in_tag }) = fmt
        add_parens = in_tag and !(List.is_empty attrs)

        add_encoded_attr = |acc, elem|
            indented =
                if add_parens then
                    up_indent (up_indent fmt)
                else
                    up_indent fmt
            acc
            |> append_if_pretty indented '\n'
            |> append_indent indented
            |> append_if_compact indented ' '
            |> Encode.append_with elem (set_in_tag indented Bool.true)

        if add_parens then
            bytes
            |> List.append '('
            |> append_if_pretty (up_indent fmt) '\n'
            |> append_indent (up_indent fmt)
            |> List.concat (Str.to_utf8 tag)
            |> |new_bytes| List.walk attrs new_bytes add_encoded_attr
            |> append_if_pretty fmt '\n'
            |> append_indent fmt
            |> List.append ')'
        else
            bytes
            |> List.concat (Str.to_utf8 tag)
            |> |new_bytes| List.walk attrs new_bytes add_encoded_attr

expect
    # Compact tag encoding
    tagged = Foo 1 2
    actual = Encode.to_bytes tagged compact
    expected = Str.to_utf8 "Foo 1 2"
    actual == expected

expect
    # Add parens in compact encoding when tag is nested in other tag
    tagged = Foo (Bar 1) (Baz 2 3)
    actual = Encode.to_bytes tagged compact |> Str.from_utf8
    expected = Ok "Foo (Bar 1) (Baz 2 3)"
    actual == expected

expect
    # Don't add parents if nested tag has no parameters
    tagged = Foo Bar
    actual = Encode.to_bytes tagged compact |> Str.from_utf8
    expected = Ok "Foo Bar"
    actual == expected

expect
    # Don't add parens for tag nested in list nested in tag
    tagged = Foo [Bar]
    actual = Encode.to_bytes tagged compact |> Str.from_utf8
    expected = Ok "Foo [Bar,]"
    actual == expected

expect
    # Don't add parens for tag nested in tuple nested in tag
    tagged = Foo (Bar, 4)
    actual = Encode.to_bytes tagged compact |> Str.from_utf8
    expected = Ok "Foo (Bar,4,)"
    actual == expected

expect
    # Don't add parens for tag nested in recored nested in tag
    tagged = Foo { x: Bar }
    actual = Encode.to_bytes tagged compact |> Str.from_utf8
    expected = Ok "Foo {x:Bar,}"
    actual == expected

expect
    # Add parens in pretty encoding when tag is nested in other tag
    tagged =
        Foo
            (
                Bar
                    1
            )
            (
                Baz
                    2
                    3
            )
    actual = Encode.to_bytes tagged pretty |> Str.from_utf8
    expected = Ok
        """
        Foo
            (
                Bar
                    1
            )
            (
                Baz
                    2
                    3
            )
        """
    actual == expected

expect
    # Pretty tag encoding
    tagged = Foo 1 2
    actual = Encode.to_bytes tagged pretty
    expected = Str.to_utf8
        """
        Foo
            1
            2
        """
    actual == expected

expect
    # Pretty encoding of nested structure
    nested =
        Foo
            0
            {
                items: (
                    "hi",
                    [
                        2,
                        3,
                        4,
                    ],
                ),
            }
    actual = Encode.to_bytes nested pretty |> Str.from_utf8
    expected =
        Ok
            """
            Foo
                0
                {
                    items: (
                        "hi",
                        [
                            2,
                            3,
                            4,
                        ],
                    ),
                }
            """
    actual == expected

decode_utf8_bytes = |bytes, from_str, len|
    { before, others } = List.split_at bytes len
    result =
        before
        |> Str.from_utf8
        |> Result.try from_str
        |> Result.map_err (|_| TooShort)
    { result, rest: others }

decode_int = |bytes, from_str|
    count_until = |list, pred|
        List.walk_until
            list
            0
            (|count, elem| if pred elem then Continue (count + 1) else Break count)

    when bytes is
        ['-', '0', 'b', .. as digits] ->
            decode_utf8_bytes bytes from_str (3 + count_until digits is_binary_digit)

        ['0', 'b', .. as digits] ->
            decode_utf8_bytes bytes from_str (2 + count_until digits is_binary_digit)

        ['-', '0', 'x', .. as digits] ->
            decode_utf8_bytes bytes from_str (3 + count_until digits is_hex_digit)

        ['0', 'x', .. as digits] ->
            decode_utf8_bytes bytes from_str (2 + count_until digits is_hex_digit)

        ['-', .. as digits] ->
            decode_utf8_bytes bytes from_str (1 + count_until digits is_decimal_digit)

        digits ->
            decode_utf8_bytes bytes from_str (count_until digits is_decimal_digit)

is_binary_digit : U8 -> Bool
is_binary_digit = |byte| byte == '_' or byte == '0' or byte == '1'

is_decimal_digit : U8 -> Bool
is_decimal_digit = |byte| byte == '_' or (byte >= '0' and byte <= '9')

is_hex_digit : U8 -> Bool
is_hex_digit = |byte|
    (byte == '_')
    or (byte >= '0' and byte <= '9')
    or (byte >= 'a' and byte <= 'f')
    or (byte >= 'A' and byte <= 'F')

decode_u8 : Decoder U8 Rvn
decode_u8 = to_decoder |bytes, @Rvn _, _| decode_int bytes Str.to_u8

expect
    # Parse decimal numbers
    bytes = Str.to_utf8 "23X"
    expected = { result: Ok (Num.to_u8 23), rest: ['X'] }
    actual = Decode.from_bytes_partial bytes compact
    actual == expected

expect
    # Parse binary numbers
    bytes = Str.to_utf8 "0b101X"
    expected = { result: Ok (Num.to_u8 5), rest: ['X'] }
    actual = Decode.from_bytes_partial bytes compact
    actual == expected

expect
    # Parse hex numbers
    bytes = Str.to_utf8 "0x1aX"
    expected = { result: Ok (Num.to_u8 26), rest: ['X'] }
    actual = Decode.from_bytes_partial bytes compact
    actual == expected

expect
    # Ignore surrounding whitespace
    bytes = Str.to_utf8 " 2 X"
    expected = { result: Ok (Num.to_u8 2), rest: ['X'] }
    actual = Decode.from_bytes_partial bytes compact
    actual == expected

expect
    # Fail attempt to decode too large a number into a U8
    bytes = Str.to_utf8 "999"
    expected : DecodeResult U8
    expected = { result: Err TooShort, rest: [] }
    actual = Decode.from_bytes_partial bytes compact
    actual == expected

expect
    # Fail if no number digits present
    bytes = ['X']
    expected : DecodeResult U8
    expected = { result: Err TooShort, rest: ['X'] }
    actual = Decode.from_bytes_partial bytes compact
    actual == expected

decode_u16 : Decoder U16 Rvn
decode_u16 = to_decoder |bytes, @Rvn _, _| decode_int bytes Str.to_u16

decode_u32 : Decoder U32 Rvn
decode_u32 = to_decoder |bytes, @Rvn _, _| decode_int bytes Str.to_u32

decode_u64 : Decoder U64 Rvn
decode_u64 = to_decoder |bytes, @Rvn _, _| decode_int bytes Str.to_u64

decode_u128 : Decoder U128 Rvn
decode_u128 = to_decoder |bytes, @Rvn _, _| decode_int bytes Str.to_u128

decode_i8 : Decoder I8 Rvn
decode_i8 = to_decoder |bytes, @Rvn _, _| decode_int bytes Str.to_i8

expect
    # Parse positive numbers
    bytes = Str.to_utf8 "23X"
    expected = { result: Ok (Num.to_i8 23), rest: ['X'] }
    actual = Decode.from_bytes_partial bytes compact
    actual == expected

expect
    # Parse negative numbers
    bytes = Str.to_utf8 "-0b101X"
    expected = { result: Ok (Num.to_i8 -5), rest: ['X'] }
    actual = Decode.from_bytes_partial bytes compact
    actual == expected

expect
    # Parse negative binary numbers
    bytes = Str.to_utf8 "-23X"
    expected = { result: Ok (Num.to_i8 -23), rest: ['X'] }
    actual = Decode.from_bytes_partial bytes compact
    actual == expected

expect
    # Parse negative hex numbers
    bytes = Str.to_utf8 "-0x1aX"
    expected = { result: Ok (Num.to_i8 -26), rest: ['X'] }
    actual = Decode.from_bytes_partial bytes compact
    actual == expected

expect
    # Ignore surrounding whitespace
    bytes = Str.to_utf8 " 2 X"
    expected = { result: Ok (Num.to_i8 2), rest: ['X'] }
    actual = Decode.from_bytes_partial bytes compact
    actual == expected

decode_i16 : Decoder I16 Rvn
decode_i16 = to_decoder |bytes, @Rvn _, _| decode_int bytes Str.to_i16

decode_i32 : Decoder I32 Rvn
decode_i32 = to_decoder |bytes, @Rvn _, _| decode_int bytes Str.to_i32

decode_i64 : Decoder I64 Rvn
decode_i64 = to_decoder |bytes, @Rvn _, _| decode_int bytes Str.to_i64

decode_i128 : Decoder I128 Rvn
decode_i128 = to_decoder |bytes, @Rvn _, _| decode_int bytes Str.to_i128

decode_float = |bytes, from_str|
    count_minus_sign =
        when bytes is
            ['-', ..] -> 1
            _ -> 0

    count_digits = |start_offset|
        List.walk_until
            (List.drop_first bytes start_offset)
            start_offset
            (|offset, byte|
                if is_decimal_digit byte then
                    Continue (offset + 1)
                else
                    Break offset
            )

    count_fractional_digits = |offset|
        when List.drop_first bytes offset is
            ['.', ..] -> count_digits (1 + offset)
            _ -> offset

    len =
        count_minus_sign
        |> count_digits
        |> count_fractional_digits

    decode_utf8_bytes bytes from_str len

decode_f32 : Decoder F32 Rvn
decode_f32 = to_decoder |bytes, @Rvn _, _| decode_float bytes Str.to_f32

decode_f64 : Decoder F64 Rvn
decode_f64 = to_decoder |bytes, @Rvn _, _| decode_float bytes Str.to_f64

decode_dec : Decoder Dec Rvn
decode_dec = to_decoder |bytes, @Rvn _, _| decode_float bytes Str.to_dec

expect
    # Parse positive numbers
    bytes = Str.to_utf8 "23X"
    n : Dec
    n = 23
    expected = { result: Ok n, rest: ['X'] }
    actual = Decode.from_bytes_partial bytes compact
    expected == actual

expect
    # Parse negative numbers
    bytes = Str.to_utf8 "-23X"
    n : Dec
    n = -23
    expected = { result: Ok n, rest: ['X'] }
    actual = Decode.from_bytes_partial bytes compact
    expected == actual

expect
    # Parse fractional numbers
    bytes = Str.to_utf8 "12.34X"
    n : Dec
    n = 12.34
    expected = { result: Ok n, rest: ['X'] }
    actual = Decode.from_bytes_partial bytes compact
    expected == actual

expect
    # Ignore surrounding whitespace
    bytes = Str.to_utf8 " 2 X"
    n : Dec
    n = 2
    expected = { result: Ok n, rest: ['X'] }
    actual = Decode.from_bytes_partial bytes compact
    expected == actual

expect
    # Fails if no number digits presence
    bytes = ['X']
    expected : DecodeResult Dec
    expected = { result: Err TooShort, rest: ['X'] }
    actual = Decode.from_bytes_partial bytes compact
    expected == actual

decode_bool : Decoder Bool Rvn
decode_bool =
    to_decoder |bytes, @Rvn _, _|
        when bytes is
            ['B', 'o', 'o', 'l', '.', 't', 'r', 'u', 'e', .. as rest] ->
                { result: Ok Bool.true, rest }

            ['B', 'o', 'o', 'l', '.', 'f', 'a', 'l', 's', 'e', .. as rest] ->
                { result: Ok Bool.false, rest }

            rest ->
                { result: Err TooShort, rest }

expect
    bytes = Str.to_utf8 "Bool.trueX"
    expected = { result: Ok Bool.true, rest: ['X'] }
    actual = Decode.from_bytes_partial bytes compact
    expected == actual

expect
    bytes = Str.to_utf8 "Bool.falseX"
    expected = { result: Ok Bool.false, rest: ['X'] }
    actual = Decode.from_bytes_partial bytes compact
    expected == actual

expect
    # Ignore surrounding whitespace
    bytes = Str.to_utf8 " Bool.false X"
    expected = { result: Ok Bool.false, rest: ['X'] }
    actual = Decode.from_bytes_partial bytes compact
    expected == actual

expect
    # Fails if neither Bool.true nor Bool.false is present
    bytes = ['X']
    expected : DecodeResult Bool
    expected = { result: Err TooShort, rest: ['X'] }
    actual = Decode.from_bytes_partial bytes compact
    expected == actual

decode_string : Decoder Str Rvn
decode_string = to_decoder |bytes, @Rvn _, _|
    append_range = |{ acc, start, len }| {
        acc: List.concat acc (List.sublist bytes { start, len }),
        start: start + len,
        len: 0,
    }

    append_byte = |{ acc, start, len }, byte| {
        acc: List.concat acc [byte],
        start,
        len,
    }

    step = |state, remaining|
        when remaining is
            ['\\', 'n', .. as rest] ->
                state
                |> append_range
                |> append_byte '\n'
                |> step rest

            ['\\', 't', .. as rest] ->
                state
                |> append_range
                |> append_byte '\t'
                |> step rest

            ['\\', '"', .. as rest] ->
                state
                |> append_range
                |> append_byte '"'
                |> step rest

            ['\\', '\\', .. as rest] ->
                state
                |> append_range
                |> append_byte '\\'
                |> step rest

            ['\\', '$', .. as rest] ->
                state
                |> append_range
                |> append_byte '$'
                |> step rest

            ['\\', 'u', '(', .. as rest] ->
                # TODO: support unicode code-point escape codes
                { result: Err TooShort, rest }

            ['\\', .. as rest] ->
                { result: Err TooShort, rest }

            ['"', .. as rest] ->
                {
                    result: state
                    |> append_range
                    |> .acc
                    |> Str.from_utf8
                    |> Result.map_err (|_| TooShort),
                    rest,
                }

            [_, .. as rest] ->
                step { state & len: state.len + 1 } rest

            [] ->
                # Ending up here means we reach end of input before the closing quote.
                {
                    result: Err TooShort,
                    rest: bytes,
                }

    when bytes is
        ['"', '"', '"', .. as rest] ->
            # TODO: support triple-quote strings
            { result: Err TooShort, rest }

        ['"', .. as rest] -> step { start: 1, len: 0, acc: [] } rest
        rest -> { result: Err TooShort, rest }

expect
    # Fails if opening quote is missing
    bytes = ['X']
    expected : DecodeResult Str
    expected = { result: Err TooShort, rest: ['X'] }
    actual = Decode.from_bytes_partial bytes compact
    expected == actual

expect
    # Parses a simple string
    bytes = ['"', 'H', 'i', '"', 'X']
    expected = { result: Ok "Hi", rest: ['X'] }
    actual = Decode.from_bytes_partial bytes compact
    expected == actual

expect
    # Ignores surrounding whitespace
    bytes = [' ', '"', 'H', 'i', '"', ' ', 'X']
    expected = { result: Ok "Hi", rest: ['X'] }
    actual = Decode.from_bytes_partial bytes compact
    expected == actual

expect
    # Parser string with special characters
    bytes = ['"', '\\', 'n', '\\', 't', '\\', '"', '\\', '\\', '\\', '$', '"']
    expected = { result: Ok "\n\t\"\\\$", rest: [] }
    actual = Decode.from_bytes_partial bytes compact
    expected == actual

expect
    # Fails for unknown escape sequence
    bytes = ['"', '\\', 'X', '"']
    expected : DecodeResult U8
    expected = { result: Err TooShort, rest: ['"', '\\', 'X', '"'] }
    actual = Decode.from_bytes_partial bytes compact
    expected == actual

expect
    # Fails if ending quote is missing
    bytes = ['"', 'H', 'i']
    expected : DecodeResult U8
    expected = { result: Err TooShort, rest: ['"', 'H', 'i'] }
    actual = Decode.from_bytes_partial bytes compact
    expected == actual

# I'd like to use skip_decoder for the record-skipping logic as well, but run
# into some errors that I think are compiler bugs, related to the `state`
# parameter. So for now I have this separate implementation for skipping
# lists.
skip_list : Decoder {} Rvn
skip_list =
    to_decoder |bytes, fmt, _|
        decode_elem : List U8 -> DecodeResult {}
        decode_elem = |remaining|
            { result, rest } = Decode.decode_with remaining skip_decoder fmt
            when result is
                Ok _ ->
                    { result: Ok {}, rest }

                Err err ->
                    { result: Err err, rest: skip_whitespace rest }

        step : List U8 -> DecodeResult {}
        step = |remaining|
            when decode_elem remaining is
                { rest: [']', .. as rest], result: _ } ->
                    {
                        result: Ok {},
                        rest,
                    }

                { rest: [',', .. as rest], result: _ } ->
                    step rest

                { rest, result: _ } ->
                    { result: Err TooShort, rest }

        when bytes is
            ['[', .. as rest] -> step rest
            rest -> { result: Err TooShort, rest }

decode_list : Decoder elem Rvn -> Decoder (List elem) Rvn
decode_list = |elem_decoder|
    to_decoder |bytes, fmt, _|
        decode_elem : List elem, List U8 -> DecodeResult (List elem)
        decode_elem = |acc, remaining|
            { result, rest } = Decode.decode_with remaining elem_decoder fmt
            when result is
                Ok elem ->
                    { result: Ok (List.append acc elem), rest }

                Err err ->
                    { result: Err err, rest: skip_whitespace rest }

        step : List elem, List U8 -> DecodeResult (List elem)
        step = |acc, remaining|
            when decode_elem acc remaining is
                { rest: [']', .. as rest], result } ->
                    {
                        result: result
                        |> Result.with_default acc
                        |> |val| Ok val,
                        rest,
                    }

                { rest: [',', .. as rest], result: Ok new_acc } ->
                    step new_acc rest

                { rest, result: _ } ->
                    { result: Err TooShort, rest }

        when bytes is
            ['[', .. as rest] -> step [] rest
            rest -> { result: Err TooShort, rest }

expect
    # Decode an empty list
    bytes = ['[', ']', 'X']
    expected : DecodeResult (List U8)
    expected = { result: Ok [], rest: ['X'] }
    actual = Decode.from_bytes_partial bytes compact
    expected == actual

expect
    # Decode a list of elements
    bytes = Str.to_utf8 "[0,1]X"
    expected : DecodeResult (List U8)
    expected = { result: Ok [0, 1], rest: ['X'] }
    actual = Decode.from_bytes_partial bytes compact
    expected == actual

expect
    # Ignores whitespace in and around elements
    bytes = Str.to_utf8 " [ 0 , 1 , ] X"
    expected : DecodeResult (List U8)
    expected = { result: Ok [0, 1], rest: ['X'] }
    actual = Decode.from_bytes_partial bytes compact
    expected == actual

expect
    # Decode a list of elements with a trailing comma
    bytes = Str.to_utf8 "[0,1,]X"
    expected : DecodeResult (List U8)
    expected = { result: Ok [0, 1], rest: ['X'] }
    actual = Decode.from_bytes_partial bytes compact
    expected == actual

expect
    # Fails if ending brace is missing
    bytes = Str.to_utf8 "[0"
    expected : DecodeResult (List U8)
    expected = { result: Err TooShort, rest: [] }
    actual = Decode.from_bytes_partial bytes compact
    expected == actual

skip_decoder : Decoder {} Rvn
skip_decoder =
    to_decoder |bytes, fmt, _|
        map_to_unit = |{ result, rest }| {
            result: Result.map_ok result (|_| {}),
            rest,
        }

        when bytes is
            ['"', ..] ->
                Decode.decode_with bytes decode_string fmt
                |> map_to_unit

            ['0', 'b', ..] ->
                Decode.decode_with bytes decode_u64 fmt
                |> map_to_unit

            ['0', 'x', ..] ->
                Decode.decode_with bytes decode_u64 fmt
                |> map_to_unit

            ['-', ..] ->
                Decode.decode_with bytes decode_f64 fmt
                |> map_to_unit

            [d, ..] if is_decimal_digit d ->
                Decode.decode_with bytes decode_f64 fmt
                |> map_to_unit

            ['B', 'o', 'o', 'l', '.', ..] ->
                Decode.decode_with bytes decode_bool fmt
                |> map_to_unit

            ['[', ..] ->
                Decode.decode_with bytes skip_list fmt

            ['(', ..] ->
                Decode.decode_with bytes skip_tuple fmt

            ['{', ..] ->
                Decode.decode_with bytes skip_record fmt

            rest ->
                # We will end up here in case of syntax errors, or when
                # attempting to decode a tag.
                { result: Err TooShort, rest }

expect
    # Skips binary numbers
    bytes = Str.to_utf8 "0b01X"
    expected : DecodeResult {}
    expected = { result: Ok {}, rest: ['X'] }
    actual = Decode.decode_with bytes skip_decoder compact
    expected == actual

expect
    # Skips hex numbers
    bytes = Str.to_utf8 "0xf1X"
    expected : DecodeResult {}
    expected = { result: Ok {}, rest: ['X'] }
    actual = Decode.decode_with bytes skip_decoder compact
    expected == actual

expect
    # Skips integers
    bytes = Str.to_utf8 "1_2X"
    expected : DecodeResult {}
    expected = { result: Ok {}, rest: ['X'] }
    actual = Decode.decode_with bytes skip_decoder compact
    expected == actual

expect
    # Skips floats
    bytes = Str.to_utf8 "-0.1X"
    expected : DecodeResult {}
    expected = { result: Ok {}, rest: ['X'] }
    actual = Decode.decode_with bytes skip_decoder compact
    expected == actual

expect
    # Skips Bool.true
    bytes = Str.to_utf8 "Bool.trueX"
    expected : DecodeResult {}
    expected = { result: Ok {}, rest: ['X'] }
    actual = Decode.decode_with bytes skip_decoder compact
    expected == actual

expect
    # Skips Bool.false
    bytes = Str.to_utf8 "Bool.falseX"
    expected : DecodeResult {}
    expected = { result: Ok {}, rest: ['X'] }
    actual = Decode.decode_with bytes skip_decoder compact
    expected == actual

expect
    # Skips lists
    bytes = Str.to_utf8 "[-0.1]X"
    expected : DecodeResult {}
    expected = { result: Ok {}, rest: ['X'] }
    actual = Decode.decode_with bytes skip_decoder compact
    expected == actual

expect
    # Skips records
    bytes = Str.to_utf8 "{a:1}X"
    expected : DecodeResult {}
    expected = { result: Ok {}, rest: ['X'] }
    actual = Decode.decode_with bytes skip_decoder compact
    expected == actual

expect
    # Skips tuples
    bytes = Str.to_utf8 "(0,1)X"
    expected : DecodeResult {}
    expected = { result: Ok {}, rest: ['X'] }
    actual = Decode.decode_with bytes skip_decoder compact
    expected == actual

# I'd like to use skip_decoder for the record-skipping logic as well, but run
# into some errors that I think are compiler bugs, related to the `state`
# parameter. So for now I have this separate implementation for skipping
# records.
skip_record : Decoder {} Rvn
skip_record =
    to_decoder |bytes, fmt, _|
        decode_key = |remaining|
            when List.split_first remaining ':' is
                Ok { before, after } -> { result: Str.from_utf8 before, rest: after }
                Err _ -> { result: Err TooShort, rest: remaining }

        decode_single_field : List U8 -> DecodeResult {}
        decode_single_field = |remaining|
            key_result = decode_key remaining
            when key_result.result is
                Err _ -> { result: Err TooShort, rest: skip_whitespace remaining }
                Ok _ -> Decode.decode_with key_result.rest skip_decoder fmt

        decode_fields = |remaining|
            when decode_single_field remaining is
                { rest: ['}', .. as rest], result: _ } ->
                    { result: Ok {}, rest }

                { rest: [',', .. as rest], result: Ok _ } ->
                    decode_fields rest

                { rest, result: _ } ->
                    { result: Err TooShort, rest }

        when bytes is
            ['{', .. as remaining] -> decode_fields remaining
            rest -> { result: Err TooShort, rest }

decode_record :
    state,
    (state, Str -> [Keep (Decoder state Rvn), Skip]),
    (state, Rvn -> Result val DecodeError)
    -> Decoder val Rvn
decode_record = |initial_state, step_field, finalizer|
    to_decoder |bytes, fmt, _|
        decode_key = |remaining|
            key_len =
                List.walk_until
                    remaining
                    0
                    (|count, byte|
                        when byte is
                            ' ' | '\t' | '\n' | '#' | ':' -> Break count
                            _ -> Continue (count + 1)
                    )
            { before, others } = List.split_at remaining key_len
            when skip_whitespace others is
                [':', .. as rest] ->
                    { result: Str.from_utf8 before, rest }

                _ ->
                    { result: Err TooShort, rest: remaining }

        decode_value : Str, state, List U8 -> DecodeResult state
        decode_value = |key, state, remaining|
            when step_field state key is
                Keep decoder -> Decode.decode_with remaining decoder fmt
                Skip ->
                    { result, rest } = Decode.decode_with remaining skip_decoder fmt
                    when result is
                        Ok _ -> { result: Ok state, rest }
                        Err err -> { result: Err err, rest }

        decode_single_field : state, List U8 -> DecodeResult state
        decode_single_field = |state, remaining|
            when decode_key (skip_whitespace remaining) is
                { result: Err _, rest } -> { result: Err TooShort, rest }
                { result: Ok key, rest } -> decode_value key state rest

        decode_fields = |state, remaining|
            when decode_single_field state remaining is
                { rest: ['}', .. as rest], result } ->
                    {
                        result: result
                        |> Result.with_default state
                        |> finalizer fmt,
                        rest,
                    }

                { rest: [',', .. as rest], result: Ok new_state } ->
                    decode_fields new_state rest

                { rest, result: _ } ->
                    { result: Err TooShort, rest }

        when bytes is
            ['{', .. as remaining] ->
                decode_fields initial_state remaining

            rest ->
                { result: Err TooShort, rest }
expect
    # Decodes an empty record
    bytes = Str.to_utf8 "{}X"
    expected = { result: Ok {}, rest: ['X'] }
    actual = Decode.from_bytes_partial bytes compact
    expected == actual

expect
    # Skips an empty record
    bytes = Str.to_utf8 "{}X"
    expected = { result: Ok {}, rest: ['X'] }
    actual = Decode.decode_with bytes skip_record compact
    expected == actual

expect
    # Decodes an empty record
    bytes = Str.to_utf8 "{ }X"
    expected = { result: Ok {}, rest: ['X'] }
    actual = Decode.from_bytes_partial bytes compact
    expected == actual

expect
    # Skips an empty record with space between
    bytes = Str.to_utf8 "{ }X"
    expected = { result: Ok {}, rest: ['X'] }
    actual = Decode.decode_with bytes skip_record compact
    expected == actual

expect
    # Decodes a record with some fields
    bytes = Str.to_utf8 "{a:1,b:2}X"
    expected = { result: Ok { a: 1, b: 2 }, rest: ['X'] }
    actual = Decode.from_bytes_partial bytes compact
    expected == actual

expect
    # Skips a record with some fields
    bytes = Str.to_utf8 "{a:1,b:2}X"
    expected = { result: Ok {}, rest: ['X'] }
    actual = Decode.decode_with bytes skip_record compact
    expected == actual

expect
    # Skips whitespace around the record and elements
    bytes = Str.to_utf8 " { a : 1 , b : 2 , } X"
    expected = { result: Ok { a: 1, b: 2 }, rest: ['X'] }
    actual = Decode.from_bytes_partial bytes compact
    expected == actual

expect
    # Skips records containing whitespace around the record and elements
    bytes = Str.to_utf8 " { a : 1 , b : 2 } X"
    expected = { result: Ok {}, rest: ['X'] }
    actual = Decode.decode_with bytes skip_record compact
    expected == actual

expect
    # Skips comment directly after key
    bytes = Str.to_utf8 "{a#hi\n : 1}X"
    expected = { result: Ok { a: 1 }, rest: ['X'] }
    actual = Decode.from_bytes_partial bytes compact
    expected == actual

expect
    # Skips a record with a comment directly after key
    bytes = Str.to_utf8 "{a#hi\n : 1}X"
    expected = { result: Ok {}, rest: ['X'] }
    actual = Decode.decode_with bytes skip_record compact
    expected == actual

expect
    # Decodes a record with a trailing comma on the last field
    bytes = Str.to_utf8 "{a:1,}X"
    expected = { result: Ok { a: 1 }, rest: ['X'] }
    actual = Decode.from_bytes_partial bytes compact
    expected == actual

expect
    # Skips a record with a trailing comma on the last field
    bytes = Str.to_utf8 "{a:1,}X"
    expected = { result: Ok {}, rest: ['X'] }
    actual = Decode.decode_with bytes skip_record compact
    expected == actual

expect
    # Skips fields not present in the expected type
    bytes = Str.to_utf8 "{a:1}X"
    expected = { result: Ok {}, rest: ['X'] }
    actual = Decode.from_bytes_partial bytes compact
    expected == actual

expect
    # Skips records with fields not present in the expected type
    bytes = Str.to_utf8 "{a:1}X"
    expected = { result: Ok {}, rest: ['X'] }
    actual = Decode.decode_with bytes skip_record compact
    expected == actual

skip_tuple : Decoder {} Rvn
skip_tuple =
    to_decoder |bytes, fmt, _|
        decode_fields = |remaining|
            field_result = Decode.decode_with remaining skip_decoder fmt
            when field_result.result is
                Ok _ ->
                    when field_result.rest is
                        [')', .. as rest] -> { result: Ok {}, rest }
                        [',', .. as rest] -> decode_fields rest
                        rest -> { result: Err TooShort, rest }

                Err err ->
                    when skip_whitespace remaining is
                        [')', .. as rest] -> { result: Ok {}, rest }
                        _ -> { result: Err err, rest: field_result.rest }

        when bytes is
            ['(', .. as remaining] -> decode_fields remaining
            rest -> { result: Err TooShort, rest }

decode_tuple :
    state,
    (state, U64 -> [Next (Decoder state Rvn), TooLong]),
    (state -> Result val DecodeError)
    -> Decoder val Rvn
decode_tuple = |initial_state, step_field, finalizer|
    to_decoder |bytes, fmt, _|
        decode_single_field : U64, state, List U8 -> DecodeResult state
        decode_single_field = |index, state, remaining|
            when step_field state index is
                Next decoder -> Decode.decode_with remaining decoder fmt
                TooLong -> { result: Err TooShort, rest: remaining }

        decode_fields = |index, state, remaining|
            field_result = decode_single_field index state remaining
            when field_result.result is
                Ok new_state ->
                    when field_result.rest is
                        [')', .. as rest] -> { result: finalizer new_state, rest }
                        [',', .. as rest] -> decode_fields (index + 1) new_state rest
                        rest -> { result: Err TooShort, rest }

                Err err ->
                    when skip_whitespace remaining is
                        [')', .. as rest] -> { result: finalizer state, rest }
                        _ -> { result: Err err, rest: field_result.rest }

        when bytes is
            ['(', .. as remaining] -> decode_fields 0 initial_state remaining
            rest -> { result: Err TooShort, rest }

expect
    # Decodes 2-tuple
    bytes = Str.to_utf8 "(1,2)X"
    expected = { result: Ok (1, 2), rest: ['X'] }
    actual = Decode.from_bytes_partial bytes compact
    expected == actual

expect
    # Decodes 3-tuple
    bytes = Str.to_utf8 "(1,2,3)X"
    expected = { result: Ok (1, 2, 3), rest: ['X'] }
    actual = Decode.from_bytes_partial bytes compact
    expected == actual

expect
    # Decodes tuple with trailing comma
    bytes = Str.to_utf8 "(1,2,)X"
    expected = { result: Ok (1, 2), rest: ['X'] }
    actual = Decode.from_bytes_partial bytes compact
    expected == actual

expect
    # Decodes tuple with whitespace surrounding it and its elements
    bytes = Str.to_utf8 " ( 1 , 2 , ) X"
    expected = { result: Ok (1, 2), rest: ['X'] }
    actual = Decode.from_bytes_partial bytes compact
    expected == actual

expect
    # Fails decoding tuple if not enough elements provided
    bytes = Str.to_utf8 "(1)X"
    expected : DecodeResult (U8, U8)
    expected = { result: Err TooShort, rest: ['X'] }
    actual = Decode.from_bytes_partial bytes compact
    expected == actual

expect
    # Fails decoding tuple if too many elments provided
    bytes = Str.to_utf8 "(1,2,3)X"
    expected : DecodeResult (U8, U8)
    expected = { result: Err TooShort, rest: ['3', ')', 'X'] }
    actual = Decode.from_bytes_partial bytes compact
    expected == actual

skip_whitespace : List U8 -> List U8
skip_whitespace = |bytes| (skip_whitespace_indent bytes).rest

skip_whitespace_indent : List U8 -> { indent : U64, rest : List U8 }
skip_whitespace_indent = |bytes|
    step : { indent : U64, rest : List U8 } -> { indent : U64, rest : List U8 }
    step = |acc|
        when acc.rest is
            [' ', .. as rest] -> step { indent: (acc.indent + 1), rest }
            ['\t', .. as rest] -> step { indent: (acc.indent + 2), rest }
            ['\n', .. as rest] -> step { indent: 0, rest }
            ['#', .. as rest] ->
                when List.split_first rest '\n' is
                    Err _ ->
                        # We reached the end of file!
                        { indent: 0, rest: [] }

                    Ok { after } ->
                        step { indent: 0, rest: after }

            _ -> acc

    step { indent: 0, rest: bytes }

expect
    # skips spaces
    bytes = Str.to_utf8 "  X"
    expected = { indent: 2, rest: ['X'] }
    actual = skip_whitespace_indent bytes
    expected == actual

expect
    # skips tabs
    bytes = Str.to_utf8 "\t\tX"
    expected = { indent: 4, rest: ['X'] }
    actual = skip_whitespace_indent bytes
    expected == actual

expect
    # skips newlinwes, which reset the indent count
    bytes = Str.to_utf8 " \n  X"
    expected = { indent: 2, rest: ['X'] }
    actual = skip_whitespace_indent bytes
    expected == actual

expect
    # skips comments up to the end of the line
    bytes = Str.to_utf8 " #c\n X"
    expected = { indent: 1, rest: ['X'] }
    actual = skip_whitespace_indent bytes
    expected == actual

# A version of to_decoder that drops surrounding whitespace.
to_decoder :
    (List U8, fmt, U64 -> DecodeResult val)
    -> Decoder val fmt
to_decoder = |decode_fn|
    Decode.custom |bytes, fmt|
        { rest, indent } = skip_whitespace_indent bytes
        decode_result = decode_fn rest fmt indent
        when decode_result.result is
            Err _ -> decode_result
            Ok val ->
                {
                    result: Ok val,
                    rest: (skip_whitespace_indent decode_result.rest).rest,
                }

up_indent : Rvn -> Rvn
up_indent = |@Rvn config|
    @Rvn { config & indent: config.indent + 1 }

append_if_compact : List U8, Rvn, U8 -> List U8
append_if_compact = |bytes, @Rvn { format }, byte|
    when format is
        Compact -> List.append bytes byte
        Pretty -> bytes

append_if_pretty : List U8, Rvn, U8 -> List U8
append_if_pretty = |bytes, @Rvn { format }, byte|
    when format is
        Compact -> bytes
        Pretty -> List.append bytes byte

append_indent : List U8, Rvn -> List U8
append_indent = |bytes, @Rvn { format, indent }|
    when format is
        Compact -> bytes
        Pretty -> List.concat bytes (List.repeat ' ' (indent * 4))

set_in_tag : Rvn, Bool -> Rvn
set_in_tag = |@Rvn config, in_tag|
    @Rvn { config & in_tag }

expect
    # Decode the README.md example (kind of, tag decoding not yet supported).
    input =
        """
        {
            language: "Roc",
            tags: ["Fast", "Friendly", "Functional"],
            color: 0x7c38f5, # supports hex digits, comments too!
        }
        """
    expected = Ok {
        language: "Roc",
        tags: ["Fast", "Friendly", "Functional"],
        color: 0x7c38f5,
    }
    actual = Decode.from_bytes (Str.to_utf8 input) compact
    actual == expected

expect
    # Decode a deeply nested structure.
    input = "{ tuple: (4, { key: [1,2,3] } ) }"
    expected = Ok { tuple: (4, { key: [1, 2, 3] }) }
    actual = Decode.from_bytes (Str.to_utf8 input) compact
    actual == expected
