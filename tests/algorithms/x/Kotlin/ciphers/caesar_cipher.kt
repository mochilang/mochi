import java.math.BigInteger

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

var default_alphabet: String = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
fun index_of(s: String, ch: String): Int {
    var i: Int = 0
    while (i < s.length) {
        if (s.substring(i, i + 1) == ch) {
            return i
        }
        i = i + 1
    }
    return 0 - 1
}

fun encrypt(input_string: String, key: Int, alphabet: String): String {
    var result: String = ""
    var i: Int = 0
    var n: Int = alphabet.length
    while (i < input_string.length) {
        var ch: String = input_string.substring(i, i + 1)
        var idx: Int = index_of(alphabet, ch)
        if (idx < 0) {
            result = result + ch
        } else {
            var new_key: BigInteger = ((Math.floorMod((idx + key), n)).toBigInteger())
            if (new_key.compareTo((0).toBigInteger()) < 0) {
                new_key = new_key.add((n).toBigInteger())
            }
            result = result + alphabet.substring((new_key).toInt(), (new_key.add((1).toBigInteger())).toInt())
        }
        i = i + 1
    }
    return result
}

fun decrypt(input_string: String, key: Int, alphabet: String): String {
    var result: String = ""
    var i: Int = 0
    var n: Int = alphabet.length
    while (i < input_string.length) {
        var ch: String = input_string.substring(i, i + 1)
        var idx: Int = index_of(alphabet, ch)
        if (idx < 0) {
            result = result + ch
        } else {
            var new_key: BigInteger = ((Math.floorMod((idx - key), n)).toBigInteger())
            if (new_key.compareTo((0).toBigInteger()) < 0) {
                new_key = new_key.add((n).toBigInteger())
            }
            result = result + alphabet.substring((new_key).toInt(), (new_key.add((1).toBigInteger())).toInt())
        }
        i = i + 1
    }
    return result
}

fun brute_force(input_string: String, alphabet: String): MutableList<String> {
    var results: MutableList<String> = mutableListOf<String>()
    var key: Int = 1
    var n: Int = alphabet.length
    while (key <= n) {
        var message: String = decrypt(input_string, key, alphabet)
        results = run { val _tmp = results.toMutableList(); _tmp.add(message); _tmp }
        key = key + 1
    }
    return results
}

fun user_main(): Unit {
    var alpha: String = default_alphabet
    var enc: String = encrypt("The quick brown fox jumps over the lazy dog", 8, alpha)
    println(enc)
    var dec: String = decrypt(enc, 8, alpha)
    println(dec)
    var brute: MutableList<String> = brute_force("jFyuMy xIH'N vLONy zILwy Gy!", alpha)
    println(brute[19]!!)
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        user_main()
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
