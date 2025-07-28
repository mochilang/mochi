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

val t: BigInteger = (_now() / 1000000000).toBigInteger()
val sec: BigInteger = t.remainder(60.toBigInteger())
val mins: BigInteger = t.divide(60.toBigInteger())
val min: BigInteger = mins.remainder(60.toBigInteger())
val hour: BigInteger = (mins.divide(60.toBigInteger())).remainder(24.toBigInteger())
var xs: String = ""
var i: Int = 0
var out: String = ""
var j: Int = 0
fun pow2(exp: Int): Int {
    var r: Int = 1
    var i: Int = 0
    while (i < exp) {
        r = r * 2
        i = i + 1
    }
    return r
}

fun bin(n: Int, digits: Int): String {
    var n: Int = n
    var s: String = ""
    var i: BigInteger = (digits - 1).toBigInteger()
    while (i.compareTo(0.toBigInteger()) >= 0) {
        val p: Int = pow2(i.toInt())
        if (n >= p) {
            s = s + "x"
            n = n - p
        } else {
            s = s + " "
        }
        if (i.compareTo(0.toBigInteger()) > 0) {
            s = s + "|"
        }
        i = i.subtract(1.toBigInteger())
    }
    return s
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println(bin(hour.toInt(), 8))
        println("")
        println(bin(min.toInt(), 8))
        println("")
        while ((i).toBigInteger().compareTo(sec) < 0) {
            xs = xs + "x"
            i = i + 1
        }
        while (j < xs.length) {
            out = out + (xs.substring(j, j + 1)).toString()
            if (((Math.floorMod((j + 1), 5)) == 0) && ((j + 1) < xs.length)) {
                out = out + "|"
            }
            j = j + 1
        }
        println(out)
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
