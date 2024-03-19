interface Rvn
    exposes [
        Rvn,
        rvn,
    ]
    imports [
        Encode.{
            Encoder,
            EncoderFormatting,
        },
    ]

Rvn := {}
    implements [
        EncoderFormatting {
            u8: encodeU8,
            u16: encodeU16,
            u32: encodeU32,
            u64: encodeU64,
            u128: encodeU128,
            i8: encodeI8,
            i16: encodeI16,
            i32: encodeI32,
            i64: encodeI64,
            i128: encodeI128,
            f32: encodeF32,
            f64: encodeF64,
            dec: encodeDec,
            bool: encodeBool,
            string: encodeString,
            list: encodeList,
            record: encodeRecord,
            tuple: encodeTuple,
            tag: encodeTag,
        },
        DecoderFormatting {
            u8: decodeU8,
            u16: decodeU16,
            u32: decodeU32,
            u64: decodeU64,
            u128: decodeU128,
            i8: decodeI8,
            i16: decodeI16,
            i32: decodeI32,
            i64: decodeI64,
            i128: decodeI128,
            f32: decodeF32,
            f64: decodeF64,
            dec: decodeDec,
            bool: decodeBool,
            string: decodeString,
            list: decodeList,
            record: decodeRecord,
            tuple: decodeTuple,
        },
    ]

rvn : Rvn
rvn = @Rvn {}

numToBytes = \n ->
    n |> Num.toStr |> Str.toUtf8

encodeU8 = \n ->
    Encode.custom \bytes, @Rvn {} ->
        List.concat bytes (numToBytes n)

encodeU16 = \n ->
    Encode.custom \bytes, @Rvn {} ->
        List.concat bytes (numToBytes n)

encodeU32 = \n ->
    Encode.custom \bytes, @Rvn {} ->
        List.concat bytes (numToBytes n)

encodeU64 = \n ->
    Encode.custom \bytes, @Rvn {} ->
        List.concat bytes (numToBytes n)

encodeU128 = \n ->
    Encode.custom \bytes, @Rvn {} ->
        List.concat bytes (numToBytes n)

encodeI8 = \n ->
    Encode.custom \bytes, @Rvn {} ->
        List.concat bytes (numToBytes n)

encodeI16 = \n ->
    Encode.custom \bytes, @Rvn {} ->
        List.concat bytes (numToBytes n)

encodeI32 = \n ->
    Encode.custom \bytes, @Rvn {} ->
        List.concat bytes (numToBytes n)

encodeI64 = \n ->
    Encode.custom \bytes, @Rvn {} ->
        List.concat bytes (numToBytes n)

encodeI128 = \n ->
    Encode.custom \bytes, @Rvn {} ->
        List.concat bytes (numToBytes n)

encodeF32 = \n ->
    Encode.custom \bytes, @Rvn {} ->
        List.concat bytes (numToBytes n)

encodeF64 = \n ->
    Encode.custom \bytes, @Rvn {} ->
        List.concat bytes (numToBytes n)

encodeDec = \n ->
    Encode.custom \bytes, @Rvn {} ->
        List.concat bytes (numToBytes n)

encodeBool = \byte ->
    Encode.custom \bytes, @Rvn {} ->
        if byte then
            List.concat bytes (Str.toUtf8 "Bool.true")
        else
            List.concat bytes (Str.toUtf8 "Bool.false")

encodeString = \str ->
    Encode.custom \bytes, @Rvn {} ->
        strBytes = Str.toUtf8 str

        encodeSliceWithoutEscaping = \{ start, len, acc } -> {
            start: start + len,
            len: 0,
            acc: List.concat acc (List.sublist strBytes { start, len }),
        }

        escape = \state, char ->
            { start, acc } = encodeSliceWithoutEscaping state

            {
                start: start + 1,
                len: 0,
                acc: List.concat acc ['\\', char],
            }

        escapeAndAppend = \state, byte ->
            when byte is
                '\n' -> escape state 'n'
                '\t' -> escape state 't'
                '"' -> escape state '"'
                '\\' -> escape state '\\'
                '$' -> escape state '$'
                _ -> { start: state.start, len: state.len + 1, acc: state.acc }

        encodedStr =
            List.walk
                strBytes
                { start: 0, len: 0, acc: bytes }
                escapeAndAppend
            |> encodeSliceWithoutEscaping
            |> .acc

        bytes
        |> List.concat ['"']
        |> List.concat encodedStr
        |> List.concat ['"']

expect
    str = "abc"
    actual = Encode.toBytes str rvn
    expected = ['"', 'a', 'b', 'c', '"']
    actual == expected

expect
    str = "a\nc"
    actual = Encode.toBytes str rvn
    expected = ['"', 'a', '\\', 'n', 'c', '"']
    actual == expected

expect
    str = "a\tc"
    actual = Encode.toBytes str rvn
    expected = ['"', 'a', '\\', 't', 'c', '"']
    actual == expected

expect
    str = "a\"c"
    actual = Encode.toBytes str rvn
    expected = ['"', 'a', '\\', '"', 'c', '"']
    actual == expected

expect
    str = "a\\c"
    actual = Encode.toBytes str rvn
    expected = ['"', 'a', '\\', '\\', 'c', '"']
    actual == expected

expect
    str = "a\$c"
    actual = Encode.toBytes str rvn
    expected = ['"', 'a', '\\', '$', 'c', '"']
    actual == expected

encodeList : List elem, (elem -> Encoder Rvn) -> Encoder Rvn
encodeList = \list, encodeElem ->
    Encode.custom \bytes, fmt ->
        addEncodedElem = \acc, elem ->
            acc
            |> Encode.appendWith (encodeElem elem) fmt
            |> List.concat [',']

        bytes
        |> List.concat ['[']
        |> \newBytes -> List.walk list newBytes addEncodedElem
        |> List.concat [']']

expect
    list : List U16
    list = []
    actual = Encode.toBytes list rvn
    expected = ['[', ']']
    actual == expected

expect
    list = [1, 2, 3]
    actual = Encode.toBytes list rvn
    expected = ['[', '1', ',', '2', ',', '3', ',', ']']
    actual == expected

encodeRecord : List { key : Str, value : Encoder Rvn } -> Encoder Rvn
encodeRecord = \fields ->
    Encode.custom \bytes, fmt ->
        addEncodedField = \acc, { key, value } ->
            acc
            |> List.concat (Str.toUtf8 key)
            |> List.concat [':']
            |> Encode.appendWith value fmt
            |> List.concat [',']

        bytes
        |> List.concat ['{']
        |> \newBytes -> List.walk fields newBytes addEncodedField
        |> List.concat ['}']

expect
    record = {}
    actual = Encode.toBytes record rvn
    expected = ['{', '}']
    actual == expected

expect
    record = { one: 1, two: 2 }
    actual = Encode.toBytes record rvn
    expected = ['{', 'o', 'n', 'e', ':', '1', ',', 't', 'w', 'o', ':', '2', ',', '}']
    actual == expected

encodeTuple : List (Encoder Rvn) -> Encoder Rvn
encodeTuple = \elems ->
    Encode.custom \bytes, fmt ->
        addEncodedElem = \acc, elem ->
            acc
            |> Encode.appendWith elem fmt
            |> List.concat [',']

        bytes
        |> List.concat ['(']
        |> \newBytes -> List.walk elems newBytes addEncodedElem
        |> List.concat [')']

expect
    tuple = (1, 2)
    actual = Encode.toBytes tuple rvn
    expected = ['(', '1', ',', '2', ',', ')']
    actual == expected

encodeTag : Str, List (Encoder Rvn) -> Encoder Rvn
encodeTag = \tag, attrs ->
    Encode.custom \bytes, fmt ->
        addEncodedAttr = \acc, elem ->
            acc
            |> List.concat ['(']
            |> Encode.appendWith elem fmt
            |> List.concat [')']

        bytes
        |> List.concat (Str.toUtf8 tag)
        |> \newBytes -> List.walk attrs newBytes addEncodedAttr

expect
    tagged = Foo 1 2
    actual = Encode.toBytes tagged rvn
    expected = ['F', 'o', 'o', '(', '1', ')', '(', '2', ')']
    actual == expected

decodeUtf8Bytes = \bytes, fromStr, len ->
    { before, others } = List.split bytes len
    result =
        before
        |> Str.fromUtf8
        |> Result.try fromStr
        |> Result.mapErr (\_ -> TooShort)
    { result, rest: others }

decodeInt = \bytes, fromStr ->
    countUntil = \list, pred ->
        List.walkUntil
            list
            0
            (\count, elem -> if pred elem then Continue (count + 1) else Break count)

    when bytes is
        ['-', '0', 'b', .. as digits] ->
            decodeUtf8Bytes bytes fromStr (3 + countUntil digits isBinaryDigit)

        ['0', 'b', .. as digits] ->
            decodeUtf8Bytes bytes fromStr (2 + countUntil digits isBinaryDigit)

        ['-', '0', 'x', .. as digits] ->
            decodeUtf8Bytes bytes fromStr (3 + countUntil digits isHexDigit)

        ['0', 'x', .. as digits] ->
            decodeUtf8Bytes bytes fromStr (2 + countUntil digits isHexDigit)

        ['-', .. as digits] ->
            decodeUtf8Bytes bytes fromStr (1 + countUntil digits isDecimalDigit)

        digits ->
            decodeUtf8Bytes bytes fromStr (countUntil digits isDecimalDigit)

isBinaryDigit : U8 -> Bool
isBinaryDigit = \byte -> byte == '_' || byte == '0' || byte == '1'

isDecimalDigit : U8 -> Bool
isDecimalDigit = \byte -> byte == '_' || (byte >= '0' && byte <= '9')

isHexDigit : U8 -> Bool
isHexDigit = \byte ->
    (byte == '_')
    || (byte >= '0' && byte <= '9')
    || (byte >= 'a' && byte <= 'f')
    || (byte >= 'A' && byte <= 'F')

decodeU8 : Decoder U8 Rvn
decodeU8 = toDecoder \bytes, @Rvn {}, _ -> decodeInt bytes Str.toU8

expect
    # Parse decimal numbers
    bytes = Str.toUtf8 "23X"
    expected = { result: Ok (Num.toU8 23), rest: ['X'] }
    actual = Decode.fromBytesPartial bytes rvn
    actual == expected

expect
    # Parse binary numbers
    bytes = Str.toUtf8 "0b101X"
    expected = { result: Ok (Num.toU8 5), rest: ['X'] }
    actual = Decode.fromBytesPartial bytes rvn
    actual == expected

expect
    # Parse hex numbers
    bytes = Str.toUtf8 "0x1aX"
    expected = { result: Ok (Num.toU8 26), rest: ['X'] }
    actual = Decode.fromBytesPartial bytes rvn
    actual == expected

expect
    # Ignore surrounding whitespace
    bytes = Str.toUtf8 " 2 X"
    expected = { result: Ok (Num.toU8 2), rest: ['X'] }
    actual = Decode.fromBytesPartial bytes rvn
    actual == expected

expect
    # Fail attempt to decode too large a number into a U8
    bytes = Str.toUtf8 "999"
    expected : DecodeResult U8
    expected = { result: Err TooShort, rest: [] }
    actual = Decode.fromBytesPartial bytes rvn
    actual == expected

expect
    # Fail if no number digits present
    bytes = ['X']
    expected : DecodeResult U8
    expected = { result: Err TooShort, rest: ['X'] }
    actual = Decode.fromBytesPartial bytes rvn
    actual == expected

decodeU16 : Decoder U16 Rvn
decodeU16 = toDecoder \bytes, @Rvn {}, _ -> decodeInt bytes Str.toU16

decodeU32 : Decoder U32 Rvn
decodeU32 = toDecoder \bytes, @Rvn {}, _ -> decodeInt bytes Str.toU32

decodeU64 : Decoder U64 Rvn
decodeU64 = toDecoder \bytes, @Rvn {}, _ -> decodeInt bytes Str.toU64

decodeU128 : Decoder U128 Rvn
decodeU128 = toDecoder \bytes, @Rvn {}, _ -> decodeInt bytes Str.toU128

decodeI8 : Decoder I8 Rvn
decodeI8 = toDecoder \bytes, @Rvn {}, _ -> decodeInt bytes Str.toI8

expect
    # Parse positive numbers
    bytes = Str.toUtf8 "23X"
    expected = { result: Ok (Num.toI8 23), rest: ['X'] }
    actual = Decode.fromBytesPartial bytes rvn
    actual == expected

expect
    # Parse negative numbers
    bytes = Str.toUtf8 "-0b101X"
    expected = { result: Ok (Num.toI8 -5), rest: ['X'] }
    actual = Decode.fromBytesPartial bytes rvn
    actual == expected

expect
    # Parse negative binary numbers
    bytes = Str.toUtf8 "-23X"
    expected = { result: Ok (Num.toI8 -23), rest: ['X'] }
    actual = Decode.fromBytesPartial bytes rvn
    actual == expected

expect
    # Parse negative hex numbers
    bytes = Str.toUtf8 "-0x1aX"
    expected = { result: Ok (Num.toI8 -26), rest: ['X'] }
    actual = Decode.fromBytesPartial bytes rvn
    actual == expected

expect
    # Ignore surrounding whitespace
    bytes = Str.toUtf8 " 2 X"
    expected = { result: Ok (Num.toI8 2), rest: ['X'] }
    actual = Decode.fromBytesPartial bytes rvn
    actual == expected

decodeI16 : Decoder I16 Rvn
decodeI16 = toDecoder \bytes, @Rvn {}, _ -> decodeInt bytes Str.toI16

decodeI32 : Decoder I32 Rvn
decodeI32 = toDecoder \bytes, @Rvn {}, _ -> decodeInt bytes Str.toI32

decodeI64 : Decoder I64 Rvn
decodeI64 = toDecoder \bytes, @Rvn {}, _ -> decodeInt bytes Str.toI64

decodeI128 : Decoder I128 Rvn
decodeI128 = toDecoder \bytes, @Rvn {}, _ -> decodeInt bytes Str.toI128

decodeFloat = \bytes, fromStr ->
    countMinusSign =
        when bytes is
            ['-', ..] -> 1
            _ -> 0

    countDigits = \startOffset ->
        List.walkUntil
            (List.dropFirst bytes startOffset)
            startOffset
            (\offset, byte ->
                if isDecimalDigit byte then
                    Continue (offset + 1)
                else
                    Break offset
            )

    countFractionalDigits = \offset ->
        when List.dropFirst bytes offset is
            ['.', .. as digits] -> countDigits (1 + offset)
            _ -> offset

    len =
        countMinusSign
        |> countDigits
        |> countFractionalDigits

    decodeUtf8Bytes bytes fromStr len

decodeF32 : Decoder F32 Rvn
decodeF32 = toDecoder \bytes, @Rvn {}, _ -> decodeFloat bytes Str.toF32

decodeF64 : Decoder F64 Rvn
decodeF64 = toDecoder \bytes, @Rvn {}, _ -> decodeFloat bytes Str.toF64

decodeDec : Decoder Dec Rvn
decodeDec = toDecoder \bytes, @Rvn {}, _ -> decodeFloat bytes Str.toDec

expect
    # Parse positive numbers
    bytes = Str.toUtf8 "23X"
    n : Dec
    n = 23
    expected = { result: Ok n, rest: ['X'] }
    actual = Decode.fromBytesPartial bytes rvn
    expected == actual

expect
    # Parse negative numbers
    bytes = Str.toUtf8 "-23X"
    n : Dec
    n = -23
    expected = { result: Ok n, rest: ['X'] }
    actual = Decode.fromBytesPartial bytes rvn
    expected == actual

expect
    # Parse fractional numbers
    bytes = Str.toUtf8 "12.34X"
    n : Dec
    n = 12.34
    expected = { result: Ok n, rest: ['X'] }
    actual = Decode.fromBytesPartial bytes rvn
    expected == actual

expect
    # Ignore surrounding whitespace
    bytes = Str.toUtf8 " 2 X"
    n : Dec
    n = 2
    expected = { result: Ok n, rest: ['X'] }
    actual = Decode.fromBytesPartial bytes rvn
    expected == actual

expect
    # Fails if no number digits presence
    bytes = ['X']
    expected : DecodeResult Dec
    expected = { result: Err TooShort, rest: ['X'] }
    actual = Decode.fromBytesPartial bytes rvn
    expected == actual

decodeBool : Decoder Bool Rvn
decodeBool =
    toDecoder \bytes, @Rvn {}, _ ->
        when bytes is
            ['B', 'o', 'o', 'l', '.', 't', 'r', 'u', 'e', .. as rest] ->
                { result: Ok Bool.true, rest }

            ['B', 'o', 'o', 'l', '.', 'f', 'a', 'l', 's', 'e', .. as rest] ->
                { result: Ok Bool.false, rest }

            rest ->
                { result: Err TooShort, rest }

expect
    bytes = Str.toUtf8 "Bool.trueX"
    expected = { result: Ok Bool.true, rest: ['X'] }
    actual = Decode.fromBytesPartial bytes rvn
    expected == actual

expect
    bytes = Str.toUtf8 "Bool.falseX"
    expected = { result: Ok Bool.false, rest: ['X'] }
    actual = Decode.fromBytesPartial bytes rvn
    expected == actual

expect
    # Ignore surrounding whitespace
    bytes = Str.toUtf8 " Bool.false X"
    expected = { result: Ok Bool.false, rest: ['X'] }
    actual = Decode.fromBytesPartial bytes rvn
    expected == actual

expect
    # Fails if neither Bool.true nor Bool.false is present
    bytes = ['X']
    expected : DecodeResult Bool
    expected = { result: Err TooShort, rest: ['X'] }
    actual = Decode.fromBytesPartial bytes rvn
    expected == actual

decodeString : Decoder Str Rvn
decodeString = toDecoder \bytes, @Rvn {}, _ ->
    appendRange = \{ acc, start, len } -> {
        acc: List.concat acc (List.sublist bytes { start, len }),
        start: start + len,
        len: 0,
    }

    appendByte = \{ acc, start, len }, byte -> {
        acc: List.concat acc [byte],
        start,
        len,
    }

    step = \state, remaining ->
        when remaining is
            ['\\', 'n', .. as rest] ->
                state
                |> appendRange
                |> appendByte '\n'
                |> step rest

            ['\\', 't', .. as rest] ->
                state
                |> appendRange
                |> appendByte '\t'
                |> step rest

            ['\\', '"', .. as rest] ->
                state
                |> appendRange
                |> appendByte '"'
                |> step rest

            ['\\', '\\', .. as rest] ->
                state
                |> appendRange
                |> appendByte '\\'
                |> step rest

            ['\\', '$', .. as rest] ->
                state
                |> appendRange
                |> appendByte '$'
                |> step rest

            ['\\', 'u', '(', .. as rest] ->
                # TODO: support unicode code-point escape codes
                { result: Err TooShort, rest }

            ['\\', .. as rest] ->
                { result: Err TooShort, rest }

            ['"', .. as rest] ->
                {
                    result: state
                    |> appendRange
                    |> .acc
                    |> Str.fromUtf8
                    |> Result.mapErr (\_ -> TooShort),
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
    actual = Decode.fromBytesPartial bytes rvn
    expected == actual

expect
    # Parses a simple string
    bytes = ['"', 'H', 'i', '"', 'X']
    expected = { result: Ok "Hi", rest: ['X'] }
    actual = Decode.fromBytesPartial bytes rvn
    expected == actual

expect
    # Ignores surrounding whitespace
    bytes = [' ', '"', 'H', 'i', '"', ' ', 'X']
    expected = { result: Ok "Hi", rest: ['X'] }
    actual = Decode.fromBytesPartial bytes rvn
    expected == actual

expect
    # Parser string with special characters
    bytes = ['"', '\\', 'n', '\\', 't', '\\', '"', '\\', '\\', '\\', '$', '"']
    expected = { result: Ok "\n\t\"\\\$", rest: [] }
    actual = Decode.fromBytesPartial bytes rvn
    expected == actual

expect
    # Fails for unknown escape sequence
    bytes = ['"', '\\', 'X', '"']
    expected : DecodeResult U8
    expected = { result: Err TooShort, rest: ['"', '\\', 'X', '"'] }
    actual = Decode.fromBytesPartial bytes rvn
    expected == actual

expect
    # Fails if ending quote is missing
    bytes = ['"', 'H', 'i']
    expected : DecodeResult U8
    expected = { result: Err TooShort, rest: ['"', 'H', 'i'] }
    actual = Decode.fromBytesPartial bytes rvn
    expected == actual

decodeList : Decoder elem Rvn -> Decoder (List elem) Rvn
decodeList = \elemDecoder ->
    toDecoder \bytes, fmt, _ ->
        decodeElem : List elem, List U8 -> DecodeResult (List elem)
        decodeElem = \acc, remaining ->
            { result, rest } = Decode.decodeWith remaining elemDecoder fmt
            when result is
                Ok elem ->
                    { result: Ok (List.append acc elem), rest }

                Err err ->
                    { result: Err err, rest: skipWhitespace rest }

        step : List elem, List U8 -> DecodeResult (List elem)
        step = \acc, remaining ->
            when decodeElem acc remaining is
                { rest: [']', .. as rest], result } ->
                    {
                        result: result
                        |> Result.withDefault acc
                        |> \val -> Ok val,
                        rest,
                    }

                { rest: [',', .. as rest], result: Ok newAcc } ->
                    step newAcc rest

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
    actual = Decode.fromBytesPartial bytes rvn
    expected == actual

expect
    # Decode a list of elements
    bytes = Str.toUtf8 "[0,1]X"
    expected : DecodeResult (List U8)
    expected = { result: Ok [0, 1], rest: ['X'] }
    actual = Decode.fromBytesPartial bytes rvn
    expected == actual

expect
    # Ignores whitespace in and around elements
    bytes = Str.toUtf8 " [ 0 , 1 , ] X"
    expected : DecodeResult (List U8)
    expected = { result: Ok [0, 1], rest: ['X'] }
    actual = Decode.fromBytesPartial bytes rvn
    expected == actual

expect
    # Decode a list of elements with a trailing comma
    bytes = Str.toUtf8 "[0,1,]X"
    expected : DecodeResult (List U8)
    expected = { result: Ok [0, 1], rest: ['X'] }
    actual = Decode.fromBytesPartial bytes rvn
    expected == actual

expect
    # Fails if ending brace is missing
    bytes = Str.toUtf8 "[0"
    expected : DecodeResult (List U8)
    expected = { result: Err TooShort, rest: [] }
    actual = Decode.fromBytesPartial bytes rvn
    expected == actual

skipDecoder : Decoder {} Rvn
skipDecoder =
    toDecoder \bytes, fmt, _ ->
        mapToUnit = \{ result, rest } -> {
            result: Result.map result (\_ -> {}),
            rest,
        }

        when bytes is
            ['"', ..] ->
                Decode.decodeWith bytes decodeString fmt
                |> mapToUnit

            ['0', 'b', ..] ->
                Decode.decodeWith bytes decodeU64 fmt
                |> mapToUnit

            ['0', 'x', ..] ->
                Decode.decodeWith bytes decodeU64 fmt
                |> mapToUnit

            ['-', ..] ->
                Decode.decodeWith bytes decodeF64 fmt
                |> mapToUnit

            [d, ..] if isDecimalDigit d ->
                Decode.decodeWith bytes decodeF64 fmt
                |> mapToUnit

            ['B', 'o', 'o', 'l', '.', ..] ->
                Decode.decodeWith bytes decodeBool fmt
                |> mapToUnit

            ['[', ..] ->
                Decode.decodeWith bytes (decodeList skipDecoder) fmt
                |> mapToUnit

            ['(', ..] ->
                Decode.decodeWith
                    bytes
                    (decodeTuple {} (\_, _ -> Next skipDecoder) (\_ -> Ok {}))
                    fmt

            ['{', ..] ->
                Decode.decodeWith bytes skipRecord fmt

            rest ->
                # We will end up here in case of syntax errors, or when
                # attempting to decode a tag.
                { result: Err TooShort, rest }

expect
    # Skips binary numbers
    bytes = Str.toUtf8 "0b01X"
    expected : DecodeResult {}
    expected = { result: Ok {}, rest: ['X'] }
    actual = Decode.decodeWith bytes skipDecoder rvn
    expected == actual

expect
    # Skips hex numbers
    bytes = Str.toUtf8 "0xf1X"
    expected : DecodeResult {}
    expected = { result: Ok {}, rest: ['X'] }
    actual = Decode.decodeWith bytes skipDecoder rvn
    expected == actual

expect
    # Skips integers
    bytes = Str.toUtf8 "1_2X"
    expected : DecodeResult {}
    expected = { result: Ok {}, rest: ['X'] }
    actual = Decode.decodeWith bytes skipDecoder rvn
    expected == actual

expect
    # Skips floats
    bytes = Str.toUtf8 "-0.1X"
    expected : DecodeResult {}
    expected = { result: Ok {}, rest: ['X'] }
    actual = Decode.decodeWith bytes skipDecoder rvn
    expected == actual

expect
    # Skips Bool.true
    bytes = Str.toUtf8 "Bool.trueX"
    expected : DecodeResult {}
    expected = { result: Ok {}, rest: ['X'] }
    actual = Decode.decodeWith bytes skipDecoder rvn
    expected == actual

expect
    # Skips Bool.false
    bytes = Str.toUtf8 "Bool.falseX"
    expected : DecodeResult {}
    expected = { result: Ok {}, rest: ['X'] }
    actual = Decode.decodeWith bytes skipDecoder rvn
    expected == actual

expect
    # Skips lists
    bytes = Str.toUtf8 "[-0.1]X"
    expected : DecodeResult {}
    expected = { result: Ok {}, rest: ['X'] }
    actual = Decode.decodeWith bytes skipDecoder rvn
    expected == actual

expect
    # Skips records
    bytes = Str.toUtf8 "{a:1}X"
    expected : DecodeResult {}
    expected = { result: Ok {}, rest: ['X'] }
    actual = Decode.decodeWith bytes skipDecoder rvn
    expected == actual

expect
    # Skips tuples
    bytes = Str.toUtf8 "(0,1)X"
    expected : DecodeResult {}
    expected = { result: Ok {}, rest: ['X'] }
    actual = Decode.decodeWith bytes skipDecoder rvn
    expected == actual

# I'd like to use skipDecoder for the record-skipping logic as well, but run
# into some errors that I think are compiler bugs, related to the `state`
# parameter. So for now I have this separate implementation for skipping
# records.
skipRecord : Decoder {} Rvn
skipRecord =
    toDecoder \bytes, fmt, _ ->
        decodeKey = \remaining ->
            when List.splitFirst remaining ':' is
                Ok { before, after } -> { result: Str.fromUtf8 before, rest: after }
                Err _ -> { result: Err TooShort, rest: remaining }

        decodeSingleField : List U8 -> DecodeResult {}
        decodeSingleField = \remaining ->
            keyResult = decodeKey remaining
            when keyResult.result is
                Err _ -> { result: Err TooShort, rest: skipWhitespace remaining }
                Ok _ -> Decode.decodeWith keyResult.rest skipDecoder fmt

        decodeFields = \remaining ->
            when decodeSingleField remaining is
                { rest: ['}', .. as rest], result: _ } ->
                    { result: Ok {}, rest }

                { rest: [',', .. as rest], result: Ok _ } ->
                    decodeFields rest

                { rest, result: _ } ->
                    { result: Err TooShort, rest }

        when bytes is
            ['{', .. as remaining] -> decodeFields remaining
            rest -> { result: Err TooShort, rest }

decodeRecord :
    state,
    (state, Str -> [Keep (Decoder state Rvn), Skip]),
    (state -> Result val DecodeError)
    -> Decoder val Rvn
decodeRecord = \initialState, stepField, finalizer ->
    toDecoder \bytes, fmt, _ ->
        decodeKey = \remaining ->
            keyLen =
                List.walkUntil
                    remaining
                    0
                    (\count, byte ->
                        when byte is
                            ' ' | '\t' | '\n' | '#' | ':' -> Break count
                            _ -> Continue (count + 1)
                    )
            { before, others } = List.split remaining keyLen
            when skipWhitespace others is
                [':', .. as rest] ->
                    { result: Str.fromUtf8 before, rest }

                _ ->
                    { result: Err TooShort, rest: remaining }

        decodeValue : Str, state, List U8 -> DecodeResult state
        decodeValue = \key, state, remaining ->
            when stepField state key is
                Keep decoder -> Decode.decodeWith remaining decoder fmt
                Skip ->
                    { result, rest } = Decode.decodeWith remaining skipDecoder fmt
                    when result is
                        Ok _ -> { result: Ok state, rest }
                        Err err -> { result: Err err, rest }

        decodeSingleField : state, List U8 -> DecodeResult state
        decodeSingleField = \state, remaining ->
            when decodeKey (skipWhitespace remaining) is
                { result: Err _, rest } -> { result: Err TooShort, rest }
                { result: Ok key, rest } -> decodeValue key state rest

        decodeFields = \state, remaining ->
            when decodeSingleField state remaining is
                { rest: ['}', .. as rest], result } ->
                    {
                        result: result
                        |> Result.withDefault state
                        |> finalizer,
                        rest,
                    }

                { rest: [',', .. as rest], result: Ok newState } ->
                    decodeFields newState rest

                { rest, result: _ } ->
                    { result: Err TooShort, rest }

        when bytes is
            ['{', .. as remaining] ->
                decodeFields initialState remaining

            rest ->
                { result: Err TooShort, rest }
expect
    # Decodes an empty record
    bytes = Str.toUtf8 "{}X"
    expected = { result: Ok {}, rest: ['X'] }
    actual = Decode.fromBytesPartial bytes rvn
    expected == actual

expect
    # Skips an empty record
    bytes = Str.toUtf8 "{}X"
    expected = { result: Ok {}, rest: ['X'] }
    actual = Decode.decodeWith bytes skipRecord rvn
    expected == actual

expect
    # Decodes an empty record
    bytes = Str.toUtf8 "{ }X"
    expected = { result: Ok {}, rest: ['X'] }
    actual = Decode.fromBytesPartial bytes rvn
    expected == actual

expect
    # Skips an empty record with space between
    bytes = Str.toUtf8 "{ }X"
    expected = { result: Ok {}, rest: ['X'] }
    actual = Decode.decodeWith bytes skipRecord rvn
    expected == actual

expect
    # Decodes a record with some fields
    bytes = Str.toUtf8 "{a:1,b:2}X"
    expected = { result: Ok { a: 1, b: 2 }, rest: ['X'] }
    actual = Decode.fromBytesPartial bytes rvn
    expected == actual

expect
    # Skips a record with some fields
    bytes = Str.toUtf8 "{a:1,b:2}X"
    expected = { result: Ok {}, rest: ['X'] }
    actual = Decode.decodeWith bytes skipRecord rvn
    expected == actual

expect
    # Skips whitespace around the record and elements
    bytes = Str.toUtf8 " { a : 1 , b : 2 , } X"
    expected = { result: Ok { a: 1, b: 2 }, rest: ['X'] }
    actual = Decode.fromBytesPartial bytes rvn
    expected == actual

expect
    # Skips records containing whitespace around the record and elements
    bytes = Str.toUtf8 " { a : 1 , b : 2 } X"
    expected = { result: Ok {}, rest: ['X'] }
    actual = Decode.decodeWith bytes skipRecord rvn
    expected == actual

expect
    # Skips comment directly after key
    bytes = Str.toUtf8 "{a#hi\n : 1}X"
    expected = { result: Ok { a: 1 }, rest: ['X'] }
    actual = Decode.fromBytesPartial bytes rvn
    expected == actual

expect
    # Skips a record with a comment directly after key
    bytes = Str.toUtf8 "{a#hi\n : 1}X"
    expected = { result: Ok {}, rest: ['X'] }
    actual = Decode.decodeWith bytes skipRecord rvn
    expected == actual

expect
    # Decodes a record with a trailing comma on the last field
    bytes = Str.toUtf8 "{a:1,}X"
    expected = { result: Ok { a: 1 }, rest: ['X'] }
    actual = Decode.fromBytesPartial bytes rvn
    expected == actual

expect
    # Skips a record with a trailing comma on the last field
    bytes = Str.toUtf8 "{a:1,}X"
    expected = { result: Ok {}, rest: ['X'] }
    actual = Decode.decodeWith bytes skipRecord rvn
    expected == actual

expect
    # Skips fields not present in the expected type
    bytes = Str.toUtf8 "{a:1}X"
    expected = { result: Ok {}, rest: ['X'] }
    actual = Decode.fromBytesPartial bytes rvn
    expected == actual

expect
    # Skips records with fields not present in the expected type
    bytes = Str.toUtf8 "{a:1}X"
    expected = { result: Ok {}, rest: ['X'] }
    actual = Decode.decodeWith bytes skipRecord rvn
    expected == actual

decodeTuple :
    state,
    (state, U64 -> [Next (Decoder state Rvn), TooLong]),
    (state -> Result val DecodeError)
    -> Decoder val Rvn
decodeTuple = \initialState, stepField, finalizer ->
    toDecoder \bytes, fmt, _ ->
        decodeSingleField : U64, state, List U8 -> DecodeResult state
        decodeSingleField = \index, state, remaining ->
            when stepField state index is
                Next decoder -> Decode.decodeWith remaining decoder fmt
                TooLong -> { result: Err TooShort, rest: remaining }

        decodeFields = \index, state, remaining ->
            fieldResult = decodeSingleField index state remaining
            when fieldResult.result is
                Ok newState ->
                    when fieldResult.rest is
                        [')', .. as rest] -> { result: finalizer newState, rest }
                        [',', .. as rest] -> decodeFields (index + 1) newState rest
                        rest -> { result: Err TooShort, rest }

                Err err ->
                    when skipWhitespace remaining is
                        [')', .. as rest] -> { result: finalizer state, rest }
                        _ -> { result: Err err, rest: fieldResult.rest }

        when bytes is
            ['(', .. as remaining] -> decodeFields 0 initialState remaining
            rest -> { result: Err TooShort, rest }

expect
    # Decodes 2-tuple
    bytes = Str.toUtf8 "(1,2)X"
    expected = { result: Ok (1, 2), rest: ['X'] }
    actual = Decode.fromBytesPartial bytes rvn
    expected == actual

expect
    # Decodes 3-tuple
    bytes = Str.toUtf8 "(1,2,3)X"
    expected = { result: Ok (1, 2, 3), rest: ['X'] }
    actual = Decode.fromBytesPartial bytes rvn
    expected == actual

expect
    # Decodes tuple with trailing comma
    bytes = Str.toUtf8 "(1,2,)X"
    expected = { result: Ok (1, 2), rest: ['X'] }
    actual = Decode.fromBytesPartial bytes rvn
    expected == actual

expect
    # Decodes tuple with whitespace surrounding it and its elements
    bytes = Str.toUtf8 " ( 1 , 2 , ) X"
    expected = { result: Ok (1, 2), rest: ['X'] }
    actual = Decode.fromBytesPartial bytes rvn
    expected == actual

expect
    # Fails decoding tuple if not enough elements provided
    bytes = Str.toUtf8 "(1)X"
    expected : DecodeResult (U8, U8)
    expected = { result: Err TooShort, rest: ['X'] }
    actual = Decode.fromBytesPartial bytes rvn
    expected == actual

expect
    # Fails decoding tuple if too many elments provided
    bytes = Str.toUtf8 "(1,2,3)X"
    expected : DecodeResult (U8, U8)
    expected = { result: Err TooShort, rest: ['3', ')', 'X'] }
    actual = Decode.fromBytesPartial bytes rvn
    expected == actual

skipWhitespace : List U8 -> List U8
skipWhitespace = \bytes -> (skipWhitespaceIndent bytes).rest

skipWhitespaceIndent : List U8 -> { indent : U64, rest : List U8 }
skipWhitespaceIndent = \bytes ->
    step : { indent : U64, rest : List U8 } -> { indent : U64, rest : List U8 }
    step = \acc ->
        when acc.rest is
            [' ', .. as rest] -> step { indent: (acc.indent + 1), rest }
            ['\t', .. as rest] -> step { indent: (acc.indent + 2), rest }
            ['\n', .. as rest] -> step { indent: 0, rest }
            ['#', .. as rest] ->
                when List.splitFirst rest '\n' is
                    Err _ ->
                        # We reached the end of file!
                        { indent: 0, rest: [] }

                    Ok { after } ->
                        step { indent: 0, rest: after }

            _ -> acc

    step { indent: 0, rest: bytes }

expect
    # skips spaces
    bytes = Str.toUtf8 "  X"
    expected = { indent: 2, rest: ['X'] }
    actual = skipWhitespaceIndent bytes
    expected == actual

expect
    # skips tabs
    bytes = Str.toUtf8 "\t\tX"
    expected = { indent: 4, rest: ['X'] }
    actual = skipWhitespaceIndent bytes
    expected == actual

expect
    # skips newlinwes, which reset the indent count
    bytes = Str.toUtf8 " \n  X"
    expected = { indent: 2, rest: ['X'] }
    actual = skipWhitespaceIndent bytes
    expected == actual

expect
    # skips comments up to the end of the line
    bytes = Str.toUtf8 " #c\n X"
    expected = { indent: 1, rest: ['X'] }
    actual = skipWhitespaceIndent bytes
    expected == actual

# A version of toDecoder that drops surrounding whitespace.
toDecoder :
    (List U8, fmt, U64 -> DecodeResult val)
    -> Decoder val fmt
toDecoder = \decodeFn ->
    Decode.custom \bytes, fmt ->
        { rest, indent } = skipWhitespaceIndent bytes
        decodeResult = decodeFn rest fmt indent
        when decodeResult.result is
            Err _ -> decodeResult
            Ok val ->
                {
                    result: Ok val,
                    rest: (skipWhitespaceIndent decodeResult.rest).rest,
                }
