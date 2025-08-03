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

fun indexOf(s: String, ch: String): Int {
    var i: Int = 0
    while (i < s.length) {
        if (s.substring(i, i + 1) == ch) {
            return i
        }
        i = i + 1
    }
    return 0 - 1
}

fun ord(ch: String): Int {
    var upper: String = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    var lower: String = "abcdefghijklmnopqrstuvwxyz"
    var idx: Int = upper.indexOf(ch)
    if (idx >= 0) {
        return 65 + idx
    }
    idx = lower.indexOf(ch)
    if (idx >= 0) {
        return 97 + idx
    }
    return 0
}

fun chr(n: Int): String {
    var upper: String = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    var lower: String = "abcdefghijklmnopqrstuvwxyz"
    if ((n >= 65) && (n < 91)) {
        return upper.substring(n - 65, n - 64)
    }
    if ((n >= 97) && (n < 123)) {
        return lower.substring(n - 97, n - 96)
    }
    return "?"
}

fun shiftRune(r: String, k: Int): String {
    if ((r >= "a") && (r <= "z")) {
        return chr((Math.floorMod(((ord(r) - 97) + k), 26)) + 97)
    }
    if ((r >= "A") && (r <= "Z")) {
        return chr((Math.floorMod(((ord(r) - 65) + k), 26)) + 65)
    }
    return r
}

fun encipher(s: String, k: Int): String {
    var out: String = ""
    var i: Int = 0
    while (i < s.length) {
        out = out + shiftRune(s.substring(i, i + 1), k)
        i = i + 1
    }
    return out
}

fun decipher(s: String, k: Int): String {
    return encipher(s, Math.floorMod((26 - (Math.floorMod(k, 26))), 26))
}

fun user_main(): Unit {
    var pt: String = "The five boxing wizards jump quickly"
    println("Plaintext: " + pt)
    for (key in mutableListOf(0, 1, 7, 25, 26)) {
        if ((key < 1) || (key > 25)) {
            println(("Key " + key.toString()) + " invalid")
            continue
        }
        var ct: String = encipher(pt, key)
        println("Key " + key.toString())
        println("  Enciphered: " + ct)
        println("  Deciphered: " + decipher(ct, key))
    }
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
