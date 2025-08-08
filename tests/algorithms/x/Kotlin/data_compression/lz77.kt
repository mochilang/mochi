import java.math.BigInteger

fun panic(msg: String): Nothing { throw RuntimeException(msg) }

var _nowSeed = 0L
var _nowSeeded = false
fun _now(): Long {
    if (!_nowSeeded) {
        System.getenv("MOCHI_NOW_SEED")?.toLongOrNull()?.let {
            _nowSeed = it
            _nowSeeded = true
        }
    }
    return if (_nowSeeded) {
        _nowSeed = (_nowSeed * 1664525 + 1013904223) % 2147483647
        kotlin.math.abs(_nowSeed)
    } else {
        kotlin.math.abs(System.nanoTime())
    }
}

fun toJson(v: Any?): String = when (v) {
    null -> "null"
    is String -> "\"" + v.replace("\"", "\\\"") + "\""
    is Boolean, is Number -> v.toString()
    is Map<*, *> -> v.entries.joinToString(prefix = "{", postfix = "}") { toJson(it.key.toString()) + ":" + toJson(it.value) }
    is Iterable<*> -> v.joinToString(prefix = "[", postfix = "]") { toJson(it) }
    else -> toJson(v.toString())
}

data class Token(var offset: Int = 0, var length: Int = 0, var indicator: String = "")
var c1: MutableList<Token> = lz77_compress("ababcbababaa", 13, 6)
fun token_to_string(t: Token): String {
    return ((((("(" + t.offset.toString()) + ", ") + t.length.toString()) + ", ") + t.indicator) + ")"
}

fun tokens_to_string(ts: MutableList<Token>): String {
    var res: String = "["
    var i: Int = 0
    while (i < ts.size) {
        res = res + token_to_string(ts[i]!!)
        if (i < (ts.size - 1)) {
            res = res + ", "
        }
        i = i + 1
    }
    return res + "]"
}

fun match_length_from_index(text: String, window: String, text_index: Int, window_index: Int): Int {
    if ((text_index >= text.length) || (window_index >= window.length)) {
        return 0
    }
    var tc: String = text.substring(text_index, text_index + 1)
    var wc: String = window.substring(window_index, window_index + 1)
    if (tc != wc) {
        return 0
    }
    return 1 + match_length_from_index(text, window + tc, text_index + 1, window_index + 1)
}

fun find_encoding_token(text: String, search_buffer: String): Token {
    if (text.length == 0) {
        panic("We need some text to work with.")
    }
    var length: Int = 0
    var offset: Int = 0
    if (search_buffer.length == 0) {
        return Token(offset = offset, length = length, indicator = text.substring(0, 1))
    }
    var i: Int = 0
    while (i < search_buffer.length) {
        var ch: String = search_buffer.substring(i, i + 1)
        var found_offset: Int = search_buffer.length - i
        if (ch == text.substring(0, 1)) {
            var found_length: Int = match_length_from_index(text, search_buffer, 0, i)
            if (found_length >= length) {
                offset = found_offset
                length = found_length
            }
        }
        i = i + 1
    }
    return Token(offset = offset, length = length, indicator = text.substring(length, length + 1))
}

fun lz77_compress(text: String, window_size: Int, lookahead: Int): MutableList<Token> {
    var search_buffer_size: Int = window_size - lookahead
    var output: MutableList<Token> = mutableListOf<Token>()
    var search_buffer: String = ""
    var remaining: String = text
    while (remaining.length > 0) {
        var token: Token = find_encoding_token(remaining, search_buffer)
        var add_len: Int = token.length + 1
        search_buffer = search_buffer + remaining.substring(0, add_len)
        if (search_buffer.length > search_buffer_size) {
            search_buffer = search_buffer.substring(search_buffer.length - search_buffer_size, search_buffer.length)
        }
        remaining = remaining.substring(add_len, remaining.length)
        output = run { val _tmp = output.toMutableList(); _tmp.add(token); _tmp }
    }
    return output
}

fun lz77_decompress(tokens: MutableList<Token>): String {
    var output: String = ""
    for (t in tokens) {
        var i: Int = 0
        while (i < t.length) {
            output = output + output.substring(output.length - t.offset, (output.length - t.offset) + 1)
            i = i + 1
        }
        output = output + t.indicator
    }
    return output
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println(tokens_to_string(c1))
        var c2: MutableList<Token> = lz77_compress("aacaacabcabaaac", 13, 6)
        println(tokens_to_string(c2))
        var tokens_example: MutableList<Token> = mutableListOf(Token(offset = 0, length = 0, indicator = "c"), Token(offset = 0, length = 0, indicator = "a"), Token(offset = 0, length = 0, indicator = "b"), Token(offset = 0, length = 0, indicator = "r"), Token(offset = 3, length = 1, indicator = "c"), Token(offset = 2, length = 1, indicator = "d"), Token(offset = 7, length = 4, indicator = "r"), Token(offset = 3, length = 5, indicator = "d"))
        println(lz77_decompress(tokens_example))
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
