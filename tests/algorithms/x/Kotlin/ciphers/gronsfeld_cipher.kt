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

var ASCII_UPPERCASE: String = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
var ASCII_LOWERCASE: String = "abcdefghijklmnopqrstuvwxyz"
var NEG_ONE: Int = 0 - 1
fun index_of(s: String, ch: String): Int {
    var i: Int = 0
    while (i < s.length) {
        if (s.substring(i, i + 1) == ch) {
            return i
        }
        i = i + 1
    }
    return (NEG_ONE.toInt())
}

fun to_uppercase(s: String): String {
    var result: String = ""
    var i: Int = 0
    while (i < s.length) {
        var ch: String = s.substring(i, i + 1)
        var idx: Int = index_of(ASCII_LOWERCASE, ch)
        if ((idx).toBigInteger().compareTo((NEG_ONE).toBigInteger()) == 0) {
            result = result + ch
        } else {
            result = result + ASCII_UPPERCASE.substring(idx, idx + 1)
        }
        i = i + 1
    }
    return result
}

fun gronsfeld(text: String, key: String): String {
    var ascii_len: Int = ASCII_UPPERCASE.length
    var key_len: Int = key.length
    if (key_len == 0) {
        panic("integer modulo by zero")
    }
    var upper_text: String = to_uppercase(text)
    var encrypted: String = ""
    var i: Int = 0
    while (i < upper_text.length) {
        var ch: String = upper_text.substring(i, i + 1)
        var idx: Int = index_of(ASCII_UPPERCASE, ch)
        if ((idx).toBigInteger().compareTo((NEG_ONE).toBigInteger()) == 0) {
            encrypted = encrypted + ch
        } else {
            var key_idx: Int = Math.floorMod(i, key_len)
            var shift: Int = (key.substring(key_idx, key_idx + 1).toInt())
            var new_position: Int = Math.floorMod((idx + shift), ascii_len)
            encrypted = encrypted + ASCII_UPPERCASE.substring(new_position, new_position + 1)
        }
        i = i + 1
    }
    return encrypted
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println(gronsfeld("hello", "412"))
        println(gronsfeld("hello", "123"))
        println(gronsfeld("", "123"))
        println(gronsfeld("yes, ¥€$ - _!@#%?", "0"))
        println(gronsfeld("yes, ¥€$ - _!@#%?", "01"))
        println(gronsfeld("yes, ¥€$ - _!@#%?", "012"))
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
