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

var LETTERS: String = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
var LETTERS_LOWER: String = "abcdefghijklmnopqrstuvwxyz"
var key: String = "HDarji"
var message: String = "This is Harshil Darji from Dharmaj."
var key_up: String = to_upper_string(key)
var encrypted: String = ""
var key_index: Int = 0
var i: Int = 0
fun find_index(s: String, ch: String): Int {
    var i: Int = 0
    while (i < s.length) {
        if (s[i].toString() == ch) {
            return i
        }
        i = i + 1
    }
    return 0 - 1
}

fun to_upper_char(ch: String): String {
    var idx: Int = find_index(LETTERS_LOWER, ch)
    if (idx >= 0) {
        return LETTERS[idx].toString()
    }
    return ch
}

fun to_lower_char(ch: String): String {
    var idx: Int = find_index(LETTERS, ch)
    if (idx >= 0) {
        return LETTERS_LOWER[idx].toString()
    }
    return ch
}

fun is_upper(ch: String): Boolean {
    return find_index(LETTERS, ch) >= 0
}

fun to_upper_string(s: String): String {
    var res: String = ""
    var i: Int = 0
    while (i < s.length) {
        res = res + to_upper_char(s[i].toString())
        i = i + 1
    }
    return res
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        while (i < message.length) {
            var symbol: String = message[i].toString()
            var upper_symbol: String = to_upper_char(symbol)
            var num: Int = find_index(LETTERS, upper_symbol)
            if (num >= 0) {
                num = num + find_index(LETTERS, key_up[key_index].toString())
                num = Math.floorMod(num, LETTERS.length)
                if (((is_upper(symbol)) as Boolean)) {
                    encrypted = encrypted + LETTERS[num].toString()
                } else {
                    encrypted = encrypted + to_lower_char(LETTERS[num].toString())
                }
                key_index = key_index + 1
                if (key_index == key_up.length) {
                    key_index = 0
                }
            } else {
                encrypted = encrypted + symbol
            }
            i = i + 1
        }
        println(encrypted)
        var decrypted: String = ""
        key_index = 0
        i = 0
        while (i < encrypted.length) {
            var symbol: String = encrypted[i].toString()
            var upper_symbol: String = to_upper_char(symbol)
            var num: Int = find_index(LETTERS, upper_symbol)
            if (num >= 0) {
                num = num - find_index(LETTERS, key_up[key_index].toString())
                num = Math.floorMod(num, LETTERS.length)
                if (((is_upper(symbol)) as Boolean)) {
                    decrypted = decrypted + LETTERS[num].toString()
                } else {
                    decrypted = decrypted + to_lower_char(LETTERS[num].toString())
                }
                key_index = key_index + 1
                if (key_index == key_up.length) {
                    key_index = 0
                }
            } else {
                decrypted = decrypted + symbol
            }
            i = i + 1
        }
        println(decrypted)
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
