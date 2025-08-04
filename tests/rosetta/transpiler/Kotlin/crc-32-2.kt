import java.math.BigInteger

fun rshift(x: Int, n: Int): Int {
return (x.toLong() / pow2(n)).toInt()
}

fun pow2(n: Int): Long {
var v = 1L
var i = 0
while (i < n) {
v *= 2
i++
}
return v
}

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

var table: MutableList<Int> = crc32Table()
fun xor(a: Int, b: Int): Int {
    var res: Int = 0
    var bit: Int = 1
    var x: Int = a
    var y: Int = b
    while ((x > 0) || (y > 0)) {
        var abit: BigInteger = (Math.floorMod(x, 2)).toBigInteger()
        var bbit: BigInteger = (Math.floorMod(y, 2)).toBigInteger()
        if (abit.compareTo((bbit)) != 0) {
            res = res + bit
        }
        x = x / 2
        y = y / 2
        bit = bit * 2
    }
    return res
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
    if (ch == " ") {
        return 32
    }
    return 0
}

fun toHex(n: Int): String {
    var digits: String = "0123456789ABCDEF"
    if (n == 0) {
        return "0"
    }
    var v: Int = n
    var out: String = ""
    while (v > 0) {
        var d: BigInteger = (Math.floorMod(v, 16)).toBigInteger()
        out = digits.substring((d).toInt(), (d.add((1).toBigInteger())).toInt()) + out
        v = v / 16
    }
    return out
}

fun crc32Table(): MutableList<Int> {
    var table: MutableList<Int> = mutableListOf<Int>()
    var i: Int = 0
    while (i < 256) {
        var word: Int = i
        var j: Int = 0
        while (j < 8) {
            if ((Math.floorMod(word, 2)) == 1) {
                word = xor(rshift(word, 1), (3988292384L.toInt()))
            } else {
                word = rshift(word, 1)
            }
            j = j + 1
        }
        table = run { val _tmp = table.toMutableList(); _tmp.add(word); _tmp }
        i = i + 1
    }
    return table
}

fun crc32(s: String): Int {
    var crc: Long = 4294967295L
    var i: Int = 0
    while (i < s.length) {
        var c: Int = ord(s.substring(i, i + 1))
        var idx: Int = xor(((Math.floorMod(crc, (256).toLong())).toInt()), c)
        crc = ((xor(table[idx]!!, rshift((crc.toInt()), 8))).toLong())
        i = i + 1
    }
    return ((4294967295L - crc).toInt())
}

fun user_main(): Unit {
    var s: String = "The quick brown fox jumps over the lazy dog"
    var result: Int = crc32(s)
    var hex: String = toHex(result).toLowerCase()
    println(hex)
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
