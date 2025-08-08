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

var cipher_map: MutableList<String> = create_cipher_map("Goodbye!!")
var encoded: String = encipher("Hello World!!", cipher_map)
fun index_in_string(s: String, ch: String): Int {
    var i: Int = 0
    while (i < s.length) {
        if (s[i].toString() == ch) {
            return i
        }
        i = i + 1
    }
    return 0 - 1
}

fun contains_char(s: String, ch: String): Boolean {
    return index_in_string(s, ch) >= 0
}

fun is_alpha(ch: String): Boolean {
    var lower: String = "abcdefghijklmnopqrstuvwxyz"
    var upper: String = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    return ((contains_char(lower, ch) || contains_char(upper, ch)) as Boolean)
}

fun to_upper(s: String): String {
    var lower: String = "abcdefghijklmnopqrstuvwxyz"
    var upper: String = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    var res: String = ""
    var i: Int = 0
    while (i < s.length) {
        var ch: String = s[i].toString()
        var idx: Int = index_in_string(lower, ch)
        if (idx >= 0) {
            res = res + upper[idx].toString()
        } else {
            res = res + ch
        }
        i = i + 1
    }
    return res
}

fun remove_duplicates(key: String): String {
    var res: String = ""
    var i: Int = 0
    while (i < key.length) {
        var ch: String = key[i].toString()
        if ((ch == " ") || ((is_alpha(ch) && (contains_char(res, ch) == false) as Boolean))) {
            res = res + ch
        }
        i = i + 1
    }
    return res
}

fun create_cipher_map(key: String): MutableList<String> {
    var alphabet: String = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    var cleaned: String = remove_duplicates(to_upper(key))
    var cipher: MutableList<String> = mutableListOf<String>()
    var i: Int = 0
    while (i < cleaned.length) {
        cipher = run { val _tmp = cipher.toMutableList(); _tmp.add(cleaned[i].toString()); _tmp }
        i = i + 1
    }
    var offset: Int = cleaned.length
    var j: Int = cipher.size
    while (j < 26) {
        var char: String = alphabet[j - offset].toString()
        while (((contains_char(cleaned, char)) as Boolean)) {
            offset = offset - 1
            char = alphabet[j - offset].toString()
        }
        cipher = run { val _tmp = cipher.toMutableList(); _tmp.add(char); _tmp }
        j = j + 1
    }
    return cipher
}

fun index_in_list(lst: MutableList<String>, ch: String): Int {
    var i: Int = 0
    while (i < lst.size) {
        if (lst[i]!! == ch) {
            return i
        }
        i = i + 1
    }
    return 0 - 1
}

fun encipher(message: String, cipher: MutableList<String>): String {
    var alphabet: String = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    var msg: String = to_upper(message)
    var res: String = ""
    var i: Int = 0
    while (i < msg.length) {
        var ch: String = msg[i].toString()
        var idx: Int = index_in_string(alphabet, ch)
        if (idx >= 0) {
            res = res + cipher[idx]!!
        } else {
            res = res + ch
        }
        i = i + 1
    }
    return res
}

fun decipher(message: String, cipher: MutableList<String>): String {
    var alphabet: String = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    var msg: String = to_upper(message)
    var res: String = ""
    var i: Int = 0
    while (i < msg.length) {
        var ch: String = msg[i].toString()
        var idx: Int = index_in_list(cipher, ch)
        if (idx >= 0) {
            res = res + alphabet[idx].toString()
        } else {
            res = res + ch
        }
        i = i + 1
    }
    return res
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println(encoded)
        println(decipher(encoded, cipher_map))
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
