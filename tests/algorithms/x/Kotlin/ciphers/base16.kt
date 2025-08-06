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

var example1: MutableList<Int> = mutableListOf(72, 101, 108, 108, 111, 32, 87, 111, 114, 108, 100, 33)
var example2: MutableList<Int> = mutableListOf(72, 69, 76, 76, 79, 32, 87, 79, 82, 76, 68, 33)
fun base16_encode(data: MutableList<Int>): String {
    var digits: String = "0123456789ABCDEF"
    var res: String = ""
    var i: Int = 0
    while (i < data.size) {
        var b: Int = data[i]!!
        if ((b < 0) || (b > 255)) {
            panic("byte out of range")
        }
        var hi: Int = b / 16
        var lo: Int = Math.floorMod(b, 16)
        res = (res + digits.substring(hi, hi + 1)) + digits.substring(lo, lo + 1)
        i = i + 1
    }
    return res
}

fun base16_decode(data: String): MutableList<Int> {
    var digits: String = "0123456789ABCDEF"
    if ((Math.floorMod(data.length, 2)) != 0) {
        panic("Base16 encoded data is invalid: Data does not have an even number of hex digits.")
    }
    fun hex_value(ch: String): Int {
        var j: Int = 0
        while (j < 16) {
            if (digits.substring(j, j + 1) == ch) {
                return j
            }
            j = j + 1
        }
        return 0 - 1
    }

    var out: MutableList<Int> = mutableListOf<Int>()
    var i: Int = 0
    while (i < data.length) {
        var hi_char: String = data.substring(i, i + 1)
        var lo_char: String = data.substring(i + 1, i + 2)
        var hi: Int = ((hex_value(hi_char)) as Int)
        var lo: Int = ((hex_value(lo_char)) as Int)
        if ((hi < 0) || (lo < 0)) {
            panic("Base16 encoded data is invalid: Data is not uppercase hex or it contains invalid characters.")
        }
        out = run { val _tmp = out.toMutableList(); _tmp.add((hi * 16) + lo); _tmp }
        i = i + 2
    }
    return out
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println(base16_encode(example1))
        println(base16_encode(example2))
        println(base16_encode(mutableListOf<Int>()))
        println(base16_decode("48656C6C6F20576F726C6421").toString())
        println(base16_decode("48454C4C4F20574F524C4421").toString())
        println(base16_decode("").toString())
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
