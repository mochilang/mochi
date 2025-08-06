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

var encode_map: MutableMap<String, String> = mutableMapOf<String, String>("a" to ("AAAAA"), "b" to ("AAAAB"), "c" to ("AAABA"), "d" to ("AAABB"), "e" to ("AABAA"), "f" to ("AABAB"), "g" to ("AABBA"), "h" to ("AABBB"), "i" to ("ABAAA"), "j" to ("BBBAA"), "k" to ("ABAAB"), "l" to ("ABABA"), "m" to ("ABABB"), "n" to ("ABBAA"), "o" to ("ABBAB"), "p" to ("ABBBA"), "q" to ("ABBBB"), "r" to ("BAAAA"), "s" to ("BAAAB"), "t" to ("BAABA"), "u" to ("BAABB"), "v" to ("BBBAB"), "w" to ("BABAA"), "x" to ("BABAB"), "y" to ("BABBA"), "z" to ("BABBB"), " " to (" "))
var decode_map: MutableMap<String, String> = make_decode_map()
fun make_decode_map(): MutableMap<String, String> {
    var m: MutableMap<String, String> = mutableMapOf<String, String>()
    for (k in encode_map.keys) {
        (m)[(encode_map)[k] as String] = k
    }
    return m
}

fun split_spaces(s: String): MutableList<String> {
    var parts: MutableList<String> = mutableListOf<String>()
    var current: String = ""
    var i: Int = 0
    while (i < s.length) {
        var ch: String = s.substring(i, i + 1)
        if (ch == " ") {
            parts = run { val _tmp = parts.toMutableList(); _tmp.add(current); _tmp }
            current = ""
        } else {
            current = current + ch
        }
        i = i + 1
    }
    parts = run { val _tmp = parts.toMutableList(); _tmp.add(current); _tmp }
    return parts
}

fun encode(word: String): String {
    var w: String = (word.toLowerCase() as String)
    var encoded: String = ""
    var i: Int = 0
    while (i < w.length) {
        var ch: String = w.substring(i, i + 1)
        if (ch in encode_map) {
            encoded = encoded + (encode_map)[ch] as String
        } else {
            panic("encode() accepts only letters of the alphabet and spaces")
        }
        i = i + 1
    }
    return encoded
}

fun decode(coded: String): String {
    var i: Int = 0
    while (i < coded.length) {
        var ch: String = coded.substring(i, i + 1)
        if ((((ch != "A") && (ch != "B") as Boolean)) && (ch != " ")) {
            panic("decode() accepts only 'A', 'B' and spaces")
        }
        i = i + 1
    }
    var words: MutableList<String> = split_spaces(coded)
    var decoded: String = ""
    var w: Int = 0
    while (w < words.size) {
        var word: String = words[w]!!
        var j: Int = 0
        while (j < word.length) {
            var segment: String = word.substring(j, j + 5)
            decoded = decoded + (decode_map)[segment] as String
            j = j + 5
        }
        if (w < (words.size - 1)) {
            decoded = decoded + " "
        }
        w = w + 1
    }
    return decoded
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println(encode("hello"))
        println(encode("hello world"))
        println(decode("AABBBAABAAABABAABABAABBAB BABAAABBABBAAAAABABAAAABB"))
        println(decode("AABBBAABAAABABAABABAABBAB"))
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
