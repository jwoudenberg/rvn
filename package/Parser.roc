interface Parser
    exposes [
        Parser,
        toDecoder,
        fromDecoder,
        wrap,
        fail,
        try,
        withBytes,
        applyUntil,
        chompWhile,
        utf8,
    ]
    imports []

Parser val fmt := List U8, fmt -> DecodeResult val where fmt implements DecoderFormatting

toDecoder : Parser val fmt -> Decoder val fmt
toDecoder = \@Parser parseFn ->
    Decode.custom parseFn

fromDecoder : Decoder val fmt -> Parser val fmt
fromDecoder = \decoder ->
    @Parser \bytes, fmt ->
        Decode.decodeWith bytes decoder fmt

wrap : val -> Parser val fmt
wrap = \val ->
    @Parser \rest, _ ->
        { result: Ok val, rest }

fail : Parser val fmt
fail = @Parser \rest, _ ->
    { result: Err TooShort, rest }

try : Parser a fmt, (a -> Parser b fmt) -> Parser b fmt
try = \@Parser parser, next ->
    @Parser \bytes, fmt ->
        { result, rest } = parser bytes fmt
        when result is
            Err err -> { result: Err err, rest }
            Ok val ->
                (@Parser nextParser) = next val
                nextParser rest fmt

withBytes : (List U8 -> Parser val fmt) -> Parser val fmt
withBytes = \parse ->
    @Parser \bytes, fmt ->
        (@Parser parser) = parse bytes
        parser bytes fmt

applyUntil :
    state,
    (state -> Parser [Continue state, Break] fmt)
    -> Parser state fmt
applyUntil = \state, step ->
    result <- step state |> try
    when result is
        Continue nextState -> applyUntil nextState step
        Break -> wrap state

chompWhile : (U8 -> Bool) -> Parser (List U8) fmt
chompWhile = \pred ->
    @Parser \bytes, _ ->
        endIndex =
            List.walkUntil
                bytes
                0
                (\count, byte ->
                    if
                        pred byte
                    then
                        Continue (count + 1)
                    else
                        Break count
                )
        { before, others } = List.split bytes endIndex
        { result: Ok before, rest: others }

utf8 : Parser Str fmt
utf8 = @Parser \bytes, _ ->
    when Str.fromUtf8 bytes is
        Ok str -> { result: Ok str, rest: [] }
        Err _ -> { result: Err TooShort, rest: bytes }
