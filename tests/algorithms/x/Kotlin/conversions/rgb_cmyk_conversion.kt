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

fun round_int(x: Double): Int {
    return ((x + 0.5).toInt())
}

fun rgb_to_cmyk(r_input: Int, g_input: Int, b_input: Int): MutableList<Int> {
    if ((((((((((r_input < 0) || (r_input >= 256) as Boolean)) || (g_input < 0) as Boolean)) || (g_input >= 256) as Boolean)) || (b_input < 0) as Boolean)) || (b_input >= 256)) {
        panic("Expected int of the range 0..255")
    }
    var r: Double = ((r_input.toDouble())) / 255.0
    var g: Double = ((g_input.toDouble())) / 255.0
    var b: Double = ((b_input.toDouble())) / 255.0
    var max_val: Double = r
    if (g > max_val) {
        max_val = g
    }
    if (b > max_val) {
        max_val = b
    }
    var k_float: Double = 1.0 - max_val
    if (k_float == 1.0) {
        return mutableListOf(0, 0, 0, 100)
    }
    var c_float: Double = (100.0 * ((1.0 - r) - k_float)) / (1.0 - k_float)
    var m_float: Double = (100.0 * ((1.0 - g) - k_float)) / (1.0 - k_float)
    var y_float: Double = (100.0 * ((1.0 - b) - k_float)) / (1.0 - k_float)
    var k_percent: Double = 100.0 * k_float
    var c: Int = round_int(c_float)
    var m: Int = round_int(m_float)
    var y: Int = round_int(y_float)
    var k: Int = round_int(k_percent)
    return mutableListOf(c, m, y, k)
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println(rgb_to_cmyk(255, 255, 255))
        println(rgb_to_cmyk(128, 128, 128))
        println(rgb_to_cmyk(0, 0, 0))
        println(rgb_to_cmyk(255, 0, 0))
        println(rgb_to_cmyk(0, 255, 0))
        println(rgb_to_cmyk(0, 0, 255))
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
