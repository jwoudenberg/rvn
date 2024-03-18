interface Parser
    exposes [
        Parser,
        run,
        toDecoder,
        fromDecoder,
        fromFn,
        wrap,
        wrapResult,
        fail,
        map,
        try,
        onError,
        peek,
        nextByte,
        walkUntil,
        chompWhile,
        utf8,
    ]
    imports []

Parser val fmt := List U8, fmt -> DecodeResult val where fmt implements DecoderFormatting

run : Parser val fmt, List U8, fmt -> DecodeResult val
run = \@Parser parser, bytes, fmt -> parser bytes fmt

toDecoder : Parser val fmt -> Decoder val fmt
toDecoder = \@Parser parseFn ->
    Decode.custom parseFn

fromDecoder : Decoder val fmt -> Parser val fmt
fromDecoder = \decoder ->
    @Parser \bytes, fmt ->
        Decode.decodeWith bytes decoder fmt

fromFn : (List U8, fmt -> DecodeResult val) -> Parser val fmt
fromFn = \parseFn ->
    @Parser \bytes, fmt -> parseFn bytes fmt

wrap : val -> Parser val fmt
wrap = \val ->
    wrapResult (Ok val)

wrapResult : Result val DecodeError -> Parser val fmt
wrapResult = \result ->
    @Parser \rest, _ ->
        { result, rest }

fail : Parser val fmt
fail = @Parser \rest, _ ->
    { result: Err TooShort, rest }

map : Parser a fmt, (a -> b) -> Parser b fmt
map = \parser, mapFn ->
    val <- try parser
    wrap (mapFn val)

try : Parser a fmt, (a -> Parser b fmt) -> Parser b fmt
try = \@Parser parser, next ->
    @Parser \bytes, fmt ->
        { result, rest } = parser bytes fmt
        when result is
            Err err -> { result: Err err, rest }
            Ok val ->
                (@Parser nextParser) = next val
                nextParser rest fmt

onError : Parser a fmt, Parser a fmt -> Parser a fmt
onError = \@Parser parser, @Parser nextParser ->
    @Parser \bytes, fmt ->
        { result, rest } = parser bytes fmt
        when result is
            Err _ ->
                nextParser rest fmt

            Ok _ ->
                { result, rest }

walkUntil : state, (state, U8 -> [Continue state, Break]) -> Parser state fmt
walkUntil = \initialState, checkByte ->
    step : state, List U8 -> DecodeResult state
    step = \state, bytes ->
        when bytes is
            [] -> { result: Ok state, rest: [] }
            [byte, .. as rest] ->
                when checkByte state byte is
                    Break -> { result: Ok state, rest: bytes }
                    Continue newState -> step newState rest

    @Parser \bytes, _ -> step initialState bytes

nextByte : Parser [Byte U8, Eof] fmt
nextByte = @Parser \bytes, _ ->
    when bytes is
        [] -> { result: Ok Eof, rest: [] }
        [byte, .. as rest] -> { result: Ok (Byte byte), rest }

peek : Parser val fmt -> Parser val fmt
peek = \@Parser parser ->
    @Parser \bytes, fmt ->
        { result } = parser bytes fmt
        { result, rest: bytes }

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
