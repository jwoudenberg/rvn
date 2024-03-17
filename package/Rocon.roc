interface Rocon
    exposes [
        Rocon,
        rocon,
    ]
    imports [
        Encode.{
            Encoder,
            EncoderFormatting,
        },
    ]

Rocon := {}
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

rocon : Rocon
rocon = @Rocon {}

numToBytes = \n ->
    n |> Num.toStr |> Str.toUtf8

encodeU8 = \n ->
    Encode.custom \bytes, @Rocon {} ->
        List.concat bytes (numToBytes n)

encodeU16 = \n ->
    Encode.custom \bytes, @Rocon {} ->
        List.concat bytes (numToBytes n)

encodeU32 = \n ->
    Encode.custom \bytes, @Rocon {} ->
        List.concat bytes (numToBytes n)

encodeU64 = \n ->
    Encode.custom \bytes, @Rocon {} ->
        List.concat bytes (numToBytes n)

encodeU128 = \n ->
    Encode.custom \bytes, @Rocon {} ->
        List.concat bytes (numToBytes n)

encodeI8 = \n ->
    Encode.custom \bytes, @Rocon {} ->
        List.concat bytes (numToBytes n)

encodeI16 = \n ->
    Encode.custom \bytes, @Rocon {} ->
        List.concat bytes (numToBytes n)

encodeI32 = \n ->
    Encode.custom \bytes, @Rocon {} ->
        List.concat bytes (numToBytes n)

encodeI64 = \n ->
    Encode.custom \bytes, @Rocon {} ->
        List.concat bytes (numToBytes n)

encodeI128 = \n ->
    Encode.custom \bytes, @Rocon {} ->
        List.concat bytes (numToBytes n)

encodeF32 = \n ->
    Encode.custom \bytes, @Rocon {} ->
        List.concat bytes (numToBytes n)

encodeF64 = \n ->
    Encode.custom \bytes, @Rocon {} ->
        List.concat bytes (numToBytes n)

encodeDec = \n ->
    Encode.custom \bytes, @Rocon {} ->
        List.concat bytes (numToBytes n)

encodeBool = \byte ->
    Encode.custom \bytes, @Rocon {} ->
        if byte then
            List.concat bytes (Str.toUtf8 "Bool.true")
        else
            List.concat bytes (Str.toUtf8 "Bool.false")

encodeString = \str ->
    Encode.custom \bytes, @Rocon {} ->
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
    actual = Encode.toBytes str rocon
    expected = ['"', 'a', 'b', 'c', '"']
    actual == expected

expect
    str = "a\nc"
    actual = Encode.toBytes str rocon
    expected = ['"', 'a', '\\', 'n', 'c', '"']
    actual == expected

expect
    str = "a\tc"
    actual = Encode.toBytes str rocon
    expected = ['"', 'a', '\\', 't', 'c', '"']
    actual == expected

expect
    str = "a\"c"
    actual = Encode.toBytes str rocon
    expected = ['"', 'a', '\\', '"', 'c', '"']
    actual == expected

expect
    str = "a\\c"
    actual = Encode.toBytes str rocon
    expected = ['"', 'a', '\\', '\\', 'c', '"']
    actual == expected

expect
    str = "a\$c"
    actual = Encode.toBytes str rocon
    expected = ['"', 'a', '\\', '$', 'c', '"']
    actual == expected

encodeList : List elem, (elem -> Encoder Rocon) -> Encoder Rocon
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
    actual = Encode.toBytes list rocon
    expected = ['[', ']']
    actual == expected

expect
    list = [1, 2, 3]
    actual = Encode.toBytes list rocon
    expected = ['[', '1', ',', '2', ',', '3', ',', ']']
    actual == expected

encodeRecord : List { key : Str, value : Encoder Rocon } -> Encoder Rocon
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
    actual = Encode.toBytes record rocon
    expected = ['{', '}']
    actual == expected

expect
    record = { one: 1, two: 2 }
    actual = Encode.toBytes record rocon
    expected = ['{', 'o', 'n', 'e', ':', '1', ',', 't', 'w', 'o', ':', '2', ',', '}']
    actual == expected

encodeTuple : List (Encoder Rocon) -> Encoder Rocon
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
    actual = Encode.toBytes tuple rocon
    expected = ['(', '1', ',', '2', ',', ')']
    actual == expected

encodeTag : Str, List (Encoder Rocon) -> Encoder Rocon
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
    actual = Encode.toBytes tagged rocon
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

decodeU8 : Decoder U8 Rocon
decodeU8 = Decode.custom \bytes, @Rocon {} -> decodeInt bytes Str.toU8

expect
    # Parse decimal numbers
    bytes = ['2', '3', 'X']
    expected = { result: Ok (Num.toU8 23), rest: ['X'] }
    actual = Decode.fromBytesPartial bytes rocon
    actual == expected

expect
    # Parse binary numbers
    bytes = ['0', 'b', '1', '0', '1', 'X']
    expected = { result: Ok (Num.toU8 5), rest: ['X'] }
    actual = Decode.fromBytesPartial bytes rocon
    actual == expected

expect
    # Parse hex numbers
    bytes = ['0', 'x', '1', 'a', 'X']
    expected = { result: Ok (Num.toU8 26), rest: ['X'] }
    actual = Decode.fromBytesPartial bytes rocon
    actual == expected

expect
    # Fail attempt to decode too large a number into a U8
    bytes = ['9', '9', '9']
    expected : DecodeResult U8
    expected = { result: Err TooShort, rest: [] }
    actual = Decode.fromBytesPartial bytes rocon
    actual == expected

expect
    # Fail if no number digits present
    bytes = ['X']
    expected : DecodeResult U8
    expected = { result: Err TooShort, rest: ['X'] }
    actual = Decode.fromBytesPartial bytes rocon
    actual == expected

decodeU16 : Decoder U16 Rocon
decodeU16 = Decode.custom \bytes, @Rocon {} -> decodeInt bytes Str.toU16

decodeU32 : Decoder U32 Rocon
decodeU32 = Decode.custom \bytes, @Rocon {} -> decodeInt bytes Str.toU32

decodeU64 : Decoder U64 Rocon
decodeU64 = Decode.custom \bytes, @Rocon {} -> decodeInt bytes Str.toU64

decodeU128 : Decoder U128 Rocon
decodeU128 = Decode.custom \bytes, @Rocon {} -> decodeInt bytes Str.toU128

decodeI8 : Decoder I8 Rocon
decodeI8 = Decode.custom \bytes, @Rocon {} -> decodeInt bytes Str.toI8

expect
    # Parse positive numbers
    bytes = ['2', '3', 'X']
    expected = { result: Ok (Num.toI8 23), rest: ['X'] }
    actual = Decode.fromBytesPartial bytes rocon
    actual == expected

expect
    # Parse negative numbers
    bytes = ['-', '0', 'b', '1', '0', '1', 'X']
    expected = { result: Ok (Num.toI8 -5), rest: ['X'] }
    actual = Decode.fromBytesPartial bytes rocon
    actual == expected

expect
    # Parse negative binary numbers
    bytes = ['-', '2', '3', 'X']
    expected = { result: Ok (Num.toI8 -23), rest: ['X'] }
    actual = Decode.fromBytesPartial bytes rocon
    actual == expected

expect
    # Parse negative hex numbers
    bytes = ['-', '0', 'x', '1', 'a', 'X']
    expected = { result: Ok (Num.toI8 -26), rest: ['X'] }
    actual = Decode.fromBytesPartial bytes rocon
    actual == expected

decodeI16 : Decoder I16 Rocon
decodeI16 = Decode.custom \bytes, @Rocon {} -> decodeInt bytes Str.toI16

decodeI32 : Decoder I32 Rocon
decodeI32 = Decode.custom \bytes, @Rocon {} -> decodeInt bytes Str.toI32

decodeI64 : Decoder I64 Rocon
decodeI64 = Decode.custom \bytes, @Rocon {} -> decodeInt bytes Str.toI64

decodeI128 : Decoder I128 Rocon
decodeI128 = Decode.custom \bytes, @Rocon {} -> decodeInt bytes Str.toI128

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

decodeF32 : Decoder F32 Rocon
decodeF32 = Decode.custom \bytes, @Rocon {} -> decodeFloat bytes Str.toF32

decodeF64 : Decoder F64 Rocon
decodeF64 = Decode.custom \bytes, @Rocon {} -> decodeFloat bytes Str.toF64

decodeDec : Decoder Dec Rocon
decodeDec = Decode.custom \bytes, @Rocon {} -> decodeFloat bytes Str.toDec

expect
    # Parse positive numbers
    bytes = ['2', '3', 'X']
    n : Dec
    n = 23
    expected = { result: Ok n, rest: ['X'] }
    actual = Decode.fromBytesPartial bytes rocon
    expected == actual

expect
    # Parse negative numbers
    bytes = ['-', '2', '3', 'X']
    n : Dec
    n = -23
    expected = { result: Ok n, rest: ['X'] }
    actual = Decode.fromBytesPartial bytes rocon
    expected == actual

expect
    # Parse fractional numbers
    bytes = ['1', '2', '.', '3', '4', 'X']
    n : Dec
    n = 12.34
    expected = { result: Ok n, rest: ['X'] }
    actual = Decode.fromBytesPartial bytes rocon
    expected == actual

expect
    # Fails if no number digits presence
    bytes = ['X']
    expected : DecodeResult Dec
    expected = { result: Err TooShort, rest: ['X'] }
    actual = Decode.fromBytesPartial bytes rocon
    expected == actual

decodeBool : Decoder Bool Rocon
decodeBool =
    Decode.custom \bytes, @Rocon {} ->
        when bytes is
            ['B', 'o', 'o', 'l', '.', 't', 'r', 'u', 'e', .. as rest] ->
                { result: Ok Bool.true, rest }

            ['B', 'o', 'o', 'l', '.', 'f', 'a', 'l', 's', 'e', .. as rest] ->
                { result: Ok Bool.false, rest }

            rest ->
                { result: Err TooShort, rest }

expect
    bytes = ['B', 'o', 'o', 'l', '.', 't', 'r', 'u', 'e', 'X']
    expected = { result: Ok Bool.true, rest: ['X'] }
    actual = Decode.fromBytesPartial bytes rocon
    expected == actual

expect
    bytes = ['B', 'o', 'o', 'l', '.', 'f', 'a', 'l', 's', 'e', 'X']
    expected = { result: Ok Bool.false, rest: ['X'] }
    actual = Decode.fromBytesPartial bytes rocon
    expected == actual

expect
    # Fails if neither Bool.true nor Bool.false is present
    bytes = ['X']
    expected : DecodeResult Bool
    expected = { result: Err TooShort, rest: ['X'] }
    actual = Decode.fromBytesPartial bytes rocon
    expected == actual

decodeString : Decoder Str Rocon
decodeString = Decode.custom \bytes, @Rocon {} ->
    step = \acc, remaining ->
        when remaining is
            ['\\', 'n', .. as rest] -> step (List.concat acc ['\n']) rest
            ['\\', 't', .. as rest] -> step (List.concat acc ['\t']) rest
            ['\\', '"', .. as rest] -> step (List.concat acc ['"']) rest
            ['\\', '\\', .. as rest] -> step (List.concat acc ['\\']) rest
            ['\\', '$', .. as rest] -> step (List.concat acc ['$']) rest
            ['\\', 'u', '(', .. as rest] -> crash "TODO: support unicode code-point escape codes"
            ['\\', .. as rest] -> { result: Err TooShort, rest }
            ['"', .. as rest] ->
                {
                    result: Str.fromUtf8 acc |> Result.mapErr (\_ -> TooShort),
                    rest,
                }

            [byte, .. as rest] ->
                # TODO: optimize this to copy ranges of unescaped chars.
                step (List.concat acc [byte]) rest

            [] ->
                # Ending up here means we reach end of input before the closing quote.
                {
                    result: Err TooShort,
                    rest: bytes,
                }

    when bytes is
        ['"', '"', '"', .. as rest] -> crash "TODO: support triple-quote strings"
        ['"', .. as rest] -> step [] rest
        rest -> { result: Err TooShort, rest }

expect
    # Fails if opening quote is missing
    bytes = ['X']
    expected : DecodeResult Str
    expected = { result: Err TooShort, rest: ['X'] }
    actual = Decode.fromBytesPartial bytes rocon
    expected == actual

expect
    # Parses a simple string
    bytes = ['"', 'H', 'i', '"', 'X']
    expected = { result: Ok "Hi", rest: ['X'] }
    actual = Decode.fromBytesPartial bytes rocon
    expected == actual

expect
    # Parser string with special characters
    bytes = ['"', '\\', 'n', '\\', 't', '\\', '"', '\\', '\\', '\\', '$', '"']
    expected = { result: Ok "\n\t\"\\\$", rest: [] }
    actual = Decode.fromBytesPartial bytes rocon
    expected == actual

expect
    # Fails for unknown escape sequence
    bytes = ['"', '\\', 'X', '"']
    expected : DecodeResult U8
    expected = { result: Err TooShort, rest: ['"', '\\', 'X', '"'] }
    actual = Decode.fromBytesPartial bytes rocon
    expected == actual

expect
    # Fails if ending quote is missing
    bytes = ['"', 'H', 'i']
    expected : DecodeResult U8
    expected = { result: Err TooShort, rest: ['"', 'H', 'i'] }
    actual = Decode.fromBytesPartial bytes rocon
    expected == actual

decodeList : Decoder elem Rocon -> Decoder (List elem) Rocon
decodeList = \elemDecoder ->
    Decode.custom \bytes, fmt ->
        dropComma =
            \remaining ->
                when remaining is
                    [',', .. as rest] -> rest
                    rest -> rest

        step = \acc, remaining ->
            when remaining is
                [',', ']', .. as rest] -> { result: Ok acc, rest }
                [']', .. as rest] -> { result: Ok acc, rest }
                rest ->
                    next = Decode.decodeWith rest elemDecoder fmt
                    when next.result is
                        Ok elem ->
                            step (List.append acc elem) (dropComma next.rest)

                        Err err ->
                            { result: Err err, rest: next.rest }
        when bytes is
            ['[', .. as rest] -> step [] rest
            rest -> { result: Err TooShort, rest }

expect
    # Decode an empty list
    bytes = ['[', ']', 'X']
    expected : DecodeResult (List U8)
    expected = { result: Ok [], rest: ['X'] }
    actual = Decode.fromBytesPartial bytes rocon
    expected == actual

expect
    # Decode a list of elements
    bytes = ['[', '0', ',', '1', ']', 'X']
    expected : DecodeResult (List U8)
    expected = { result: Ok [0, 1], rest: ['X'] }
    actual = Decode.fromBytesPartial bytes rocon
    expected == actual

expect
    # Decode a list of elements with a trailing comma
    bytes = ['[', '0', ',', '1', ',', ']', 'X']
    expected : DecodeResult (List U8)
    expected = { result: Ok [0, 1], rest: ['X'] }
    actual = Decode.fromBytesPartial bytes rocon
    expected == actual

expect
    # Fails if ending brace is missing
    bytes = ['[', '0']
    expected : DecodeResult (List U8)
    expected = { result: Err TooShort, rest: [] }
    actual = Decode.fromBytesPartial bytes rocon
    expected == actual

skipDecoder : Decoder {} Rocon
skipDecoder =
    Decode.custom \bytes, fmt ->
        mapToUnit = \{ result, rest } -> {
            result: Result.map result (\_ -> {}),
            rest,
        }

        when bytes is
            ['"', ..] ->
                Decode.decodeWith bytes decodeString fmt
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

            ['(', ..] -> crash "UNIMPLEMENTED"
            ['{', ..] ->
                Decode.decodeWith bytes skipRecord fmt

            rest -> decodingCrash { rest, msg: "failed decoding while skipping a record field" }

decodingCrash : { rest: List U8, msg : Str } -> *
decodingCrash = \{ rest, msg } ->
  when Str.fromUtf8 rest is
    Err _ -> crash msg
    Ok str ->
      words = Str.split str " "
      excerpt = List.takeFirst words 3 |> Str.joinWith ","

      crash "\(msg): \(excerpt)..."

expect
    # Skips floats
    bytes = ['-', '0', '.', '1', 'X']
    expected : DecodeResult {}
    expected = { result: Ok {}, rest: ['X'] }
    actual = Decode.decodeWith bytes skipDecoder rocon
    expected == actual

expect
    # Skips lists
    bytes = ['[', '-', '0', '.', '1', ']', 'X']
    expected : DecodeResult {}
    expected = { result: Ok {}, rest: ['X'] }
    actual = Decode.decodeWith bytes skipDecoder rocon
    expected == actual

expect
    # Skips records
    bytes = ['{', 'a', ':', '1', '}', 'X']
    expected : DecodeResult {}
    expected = { result: Ok {}, rest: ['X'] }
    actual = Decode.decodeWith bytes skipDecoder rocon
    expected == actual

# I'd like to use skipDecoder for the record-skipping logic as well, but run
# into some errors that I think are compiler bugs, related to the `state`
# parameter. So for now I have this separate implementation for skipping
# records.
skipRecord : Decoder {} Rocon
skipRecord =
    Decode.custom \bytes, fmt ->
        decodeKey = \remaining ->
            when List.splitFirst remaining ':' is
                Ok { before, after } -> { result: Str.fromUtf8 before, rest: after }
                Err _ -> { result: Err TooShort, rest: remaining }

        decodeSingleField : List U8 -> DecodeResult {}
        decodeSingleField = \remaining ->
            keyResult = decodeKey remaining
            when keyResult.result is
                Err _ -> { result: Err TooShort, rest: remaining }
                Ok _ -> Decode.decodeWith keyResult.rest skipDecoder fmt

        decodeFields = \remaining ->
            fieldResult = decodeSingleField remaining
            when fieldResult.result is
                Ok {} ->
                    when fieldResult.rest is
                        ['}', .. as rest] -> { result: Ok {}, rest }
                        [',', '}', .. as rest] -> { result: Ok {}, rest }
                        [',', .. as rest] -> decodeFields rest
                        rest -> { result: Err TooShort, rest }

                Err err -> { result: Err err, rest: fieldResult.rest }

        when bytes is
            ['{', .. as remaining] ->
                when remaining is
                    ['}', .. as rest] -> { result: Ok {}, rest }
                    [',', '}', .. as rest] -> { result: Ok {}, rest }
                    _ -> decodeFields remaining

            rest -> { result: Err TooShort, rest }

decodeRecord :
    state,
    (state, Str -> [Keep (Decoder state Rocon), Skip]),
    (state -> Result val DecodeError)
    -> Decoder val Rocon
decodeRecord = \initialState, stepField, finalizer ->
    Decode.custom \bytes, fmt ->
        decodeKey = \remaining ->
            when List.splitFirst remaining ':' is
                Ok { before, after } -> { result: Str.fromUtf8 before, rest: after }
                Err _ -> { result: Err TooShort, rest: remaining }

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
            keyResult = decodeKey remaining
            when keyResult.result is
                Err _ -> { result: Err TooShort, rest: remaining }
                Ok key -> decodeValue key state keyResult.rest

        decodeFields = \state, remaining ->
            fieldResult = decodeSingleField state remaining
            when fieldResult.result is
                Ok newState ->
                    when fieldResult.rest is
                        ['}', .. as rest] -> { result: finalizer newState, rest }
                        [',', '}', .. as rest] -> { result: finalizer newState, rest }
                        [',', .. as rest] -> decodeFields newState rest
                        rest -> { result: Err TooShort, rest }

                Err err -> { result: Err err, rest: fieldResult.rest }

        when bytes is
            ['{', .. as remaining] ->
                when remaining is
                    ['}', .. as rest] -> { result: finalizer initialState, rest }
                    [',', '}', .. as rest] -> { result: finalizer initialState, rest }
                    _ -> decodeFields initialState remaining

            rest -> { result: Err TooShort, rest }

expect
    # Decodes an empty record
    bytes = ['{', '}', 'X']
    expected = { result: Ok {}, rest: ['X'] }
    actual = Decode.fromBytesPartial bytes rocon
    expected == actual

expect
    # Skips an empty record
    bytes = ['{', '}', 'X']
    expected = { result: Ok {}, rest: ['X'] }
    actual = Decode.decodeWith bytes skipRecord rocon
    expected == actual

expect
    # Decodes a record with some fields
    bytes = ['{', 'a', ':', '1', ',', 'b', ':', '2', '}', 'X']
    expected = { result: Ok { a: 1, b: 2 }, rest: ['X'] }
    actual = Decode.fromBytesPartial bytes rocon
    expected == actual

expect
    # Skips a record with some fields
    bytes = ['{', 'a', ':', '1', ',', 'b', ':', '2', '}', 'X']
    expected = { result: Ok {}, rest: ['X'] }
    actual = Decode.decodeWith bytes skipRecord rocon
    expected == actual

expect
    # Decodes a record with a trailing comma on the last field
    bytes = ['{', 'a', ':', '1', ',', '}', 'X']
    expected = { result: Ok { a: 1 }, rest: ['X'] }
    actual = Decode.fromBytesPartial bytes rocon
    expected == actual

expect
    # Decodes a record with a trailing comma on the last field
    bytes = ['{', 'a', ':', '1', ',', '}', 'X']
    expected = { result: Ok {}, rest: ['X'] }
    actual = Decode.decodeWith bytes skipRecord rocon
    expected == actual

expect
    # Skips fields not present in the expected type
    bytes = ['{', 'a', ':', '1', '}', 'X']
    expected = { result: Ok {}, rest: ['X'] }
    actual = Decode.fromBytesPartial bytes rocon
    expected == actual

expect
    # Skips records with fields not present in the expected type
    bytes = ['{', 'a', ':', '1', '}', 'X']
    expected = { result: Ok {}, rest: ['X'] }
    actual = Decode.decodeWith bytes skipRecord rocon
    expected == actual

decodeTuple : state, (state, U64 -> [Next (Decoder state Rocon), TooLong]), (state -> Result val DecodeError) -> Decoder val Rocon
