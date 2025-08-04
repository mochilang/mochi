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

fun weekday(y: Int, m: Int, d: Int): Int {
    var yy: Int = y
    var mm: Int = m
    if (mm < 3) {
        mm = mm + 12
        yy = yy - 1
    }
    var k: BigInteger = (Math.floorMod(yy, 100)).toBigInteger()
    var j: Int = ((yy / 100).toInt())
    var a: Int = (((13 * (mm + 1)) / 5).toInt())
    var b: Int = ((k.divide((4).toBigInteger())).toInt())
    var c: Int = ((j / 4).toInt())
    return ((((((((d + a)).toBigInteger().add((k))).add((b).toBigInteger())).add((c).toBigInteger())).add((5 * j).toBigInteger())).remainder((7).toBigInteger())).toInt())
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        for (year in 2008 until 2122) {
            if (weekday(year, 12, 25) == 1) {
                println(("25 December " + year.toString()) + " is Sunday")
            }
        }
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
