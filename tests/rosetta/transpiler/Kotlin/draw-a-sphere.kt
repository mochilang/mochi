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

data class V3(var x: Double = 0.0, var y: Double = 0.0, var z: Double = 0.0)
fun sqrtApprox(x: Double): Double {
    if (x <= 0.0) {
        return 0.0
    }
    var guess: Double = x
    var i: Int = 0
    while (i < 20) {
        guess = (guess + (x / guess)) / 2.0
        i = i + 1
    }
    return guess
}

fun powf(base: Double, exp: Int): Double {
    var result: Double = 1.0
    var i: Int = 0
    while (i < exp) {
        result = result * base
        i = i + 1
    }
    return result
}

fun normalize(v: V3): V3 {
    var len: Double = sqrtApprox(((v.x * v.x) + (v.y * v.y)) + (v.z * v.z))
    return V3(x = v.x / len, y = v.y / len, z = v.z / len)
}

fun dot(a: V3, b: V3): Double {
    var d: Double = ((a.x * b.x) + (a.y * b.y)) + (a.z * b.z)
    if (d < 0.0) {
        return 0.0 - d
    }
    return 0.0
}

fun drawSphere(r: Int, k: Int, ambient: Double, light: V3, shades: String): Unit {
    var i: Int = 0 - r
    while (i <= r) {
        var x: Double = ((i.toDouble())) + 0.5
        var line: String = ""
        var j: BigInteger = ((0 - (2 * r)).toBigInteger())
        while (j.compareTo((2 * r).toBigInteger()) <= 0) {
            var y: Double = (((j.toDouble())) / 2.0) + 0.5
            if (((x * x) + (y * y)) <= (((r.toDouble())) * ((r.toDouble())))) {
                var zsq: Double = ((((r.toDouble())) * ((r.toDouble()))) - (x * x)) - (y * y)
                var vec: V3 = normalize(V3(x = x, y = y, z = sqrtApprox(zsq)))
                var b: Double = powf(dot(light, vec), k) + ambient
                var intensity: Int = (((1.0 - b) * (((shades.length.toDouble())) - 1.0)).toInt())
                if (intensity < 0) {
                    intensity = 0
                }
                if (intensity >= shades.length) {
                    intensity = shades.length - 1
                }
                line = line + shades.substring(intensity, intensity + 1)
            } else {
                line = line + " "
            }
            j = j.add((1).toBigInteger())
        }
        println(line)
        i = i + 1
    }
}

fun user_main(): Unit {
    var shades: String = ".:!*oe&#%@"
    var light: V3 = normalize(V3(x = 30.0, y = 30.0, z = 0.0 - 50.0))
    drawSphere(20, 4, 0.1, light, shades)
    drawSphere(10, 2, 0.4, light, shades)
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
