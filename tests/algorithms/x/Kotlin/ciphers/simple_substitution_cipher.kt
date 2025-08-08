import java.math.BigInteger

fun <T> _listSet(lst: MutableList<T>, idx: Int, v: T) { while (lst.size <= idx) lst.add(v); lst[idx] = v }

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
var LOWERCASE: String = "abcdefghijklmnopqrstuvwxyz"
var seed: Int = 1
var key: String = "LFWOAYUISVKMNXPBDCRJTQEGHZ"
fun rand(n: Int): Int {
    seed = Math.floorMod(((seed * 1664525) + 1013904223), 2147483647)
    return Math.floorMod(seed, n)
}

fun get_random_key(): String {
    var chars: MutableList<String> = mutableListOf<String>()
    var i: Int = 0
    while (i < LETTERS.length) {
        chars = run { val _tmp = chars.toMutableList(); _tmp.add(LETTERS[i].toString()); _tmp }
        i = i + 1
    }
    var j: BigInteger = ((chars.size - 1).toBigInteger())
    while (j.compareTo((0).toBigInteger()) > 0) {
        var k: Int = rand(((j.add((1).toBigInteger())).toInt()))
        var tmp: String = chars[(j).toInt()]!!
        _listSet(chars, (j).toInt(), chars[k]!!)
        _listSet(chars, k, tmp)
        j = j.subtract((1).toBigInteger())
    }
    var res: String = ""
    i = 0
    while (i < chars.size) {
        res = res + chars[i]!!
        i = i + 1
    }
    return res
}

fun check_valid_key(key: String): Boolean {
    if (key.length != LETTERS.length) {
        return false
    }
    var used: MutableMap<String, Boolean> = mutableMapOf<String, Boolean>()
    var i: Int = 0
    while (i < key.length) {
        var ch: String = key[i].toString()
        if ((((used)[ch] as Boolean) as Boolean)) {
            return false
        }
        (used)[ch] = true
        i = i + 1
    }
    i = 0
    while (i < LETTERS.length) {
        var ch: String = LETTERS[i].toString()
        if (!(((used)[ch] as Boolean) as? Boolean ?: false)) {
            return false
        }
        i = i + 1
    }
    return true
}

fun index_in(s: String, ch: String): Int {
    var i: Int = 0
    while (i < s.length) {
        if (s[i].toString() == ch) {
            return i
        }
        i = i + 1
    }
    return 0 - 1
}

fun char_to_upper(c: String): String {
    var i: Int = 0
    while (i < LOWERCASE.length) {
        if (c == LOWERCASE[i].toString()) {
            return LETTERS[i].toString()
        }
        i = i + 1
    }
    return c
}

fun char_to_lower(c: String): String {
    var i: Int = 0
    while (i < LETTERS.length) {
        if (c == LETTERS[i].toString()) {
            return LOWERCASE[i].toString()
        }
        i = i + 1
    }
    return c
}

fun is_upper(c: String): Boolean {
    var i: Int = 0
    while (i < LETTERS.length) {
        if (c == LETTERS[i].toString()) {
            return true
        }
        i = i + 1
    }
    return false
}

fun translate_message(key: String, message: String, mode: String): String {
    var chars_a: String = LETTERS
    var chars_b: String = key
    if (mode == "decrypt") {
        var tmp: String = chars_a
        chars_a = chars_b
        chars_b = tmp
    }
    var translated: String = ""
    var i: Int = 0
    while (i < message.length) {
        var symbol: String = message[i].toString()
        var upper_symbol: String = char_to_upper(symbol)
        var idx: Int = index_in(chars_a, upper_symbol)
        if (idx >= 0) {
            var mapped: String = chars_b[idx].toString()
            if (((is_upper(symbol)) as Boolean)) {
                translated = translated + mapped
            } else {
                translated = translated + char_to_lower(mapped)
            }
        } else {
            translated = translated + symbol
        }
        i = i + 1
    }
    return translated
}

fun encrypt_message(key: String, message: String): String {
    var res: String = translate_message(key, message, "encrypt")
    return res
}

fun decrypt_message(key: String, message: String): String {
    var res: String = translate_message(key, message, "decrypt")
    return res
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println(encrypt_message(key, "Harshil Darji"))
        println(decrypt_message(key, "Ilcrism Olcvs"))
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
