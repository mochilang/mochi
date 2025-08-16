import java.math.BigInteger

fun panic(msg: String): Nothing { throw RuntimeException(msg) }

fun _numToStr(v: Number): String {
    val d = v.toDouble()
    val i = d.toLong()
    return if (d == i.toDouble()) i.toString() else d.toString()
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

fun gcd(a: Int, b: Int): Int {
    var x: Int = (a).toInt()
    var y: Int = (b).toInt()
    while (y != 0) {
        var r: Int = (Math.floorMod(x, y)).toInt()
        x = y
        y = r
    }
    if (x < 0) {
        return 0 - x
    }
    return x
}

fun get_greatest_common_divisor(nums: MutableList<Int>): Int {
    if (nums.size == 0) {
        panic("at least one number is required")
    }
    var g: Int = (nums[0]!!).toInt()
    if (g <= 0) {
        panic("numbers must be integer and greater than zero")
    }
    var i: Int = (1).toInt()
    while (i < nums.size) {
        var n: Int = (nums[i]!!).toInt()
        if (n <= 0) {
            panic("numbers must be integer and greater than zero")
        }
        g = gcd(g, n)
        i = i + 1
    }
    return g
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println(_numToStr(get_greatest_common_divisor(mutableListOf(18, 45))))
        println(_numToStr(get_greatest_common_divisor(mutableListOf(23, 37))))
        println(_numToStr(get_greatest_common_divisor(mutableListOf(2520, 8350))))
        println(_numToStr(get_greatest_common_divisor(mutableListOf(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))))
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
