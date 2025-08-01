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

data class Vector(var x: Double, var y: Double, var z: Double)
fun add(a: Vector, b: Vector): Vector {
    return Vector(x = a.x + b.x, y = a.y + b.y, z = a.z + b.z)
}

fun sub(a: Vector, b: Vector): Vector {
    return Vector(x = a.x - b.x, y = a.y - b.y, z = a.z - b.z)
}

fun mul(v: Vector, s: Double): Vector {
    return Vector(x = v.x * s, y = v.y * s, z = v.z * s)
}

fun dot(a: Vector, b: Vector): Double {
    return ((a.x * b.x) + (a.y * b.y)) + (a.z * b.z)
}

fun intersectPoint(rv: Vector, rp: Vector, pn: Vector, pp: Vector): Vector {
    var diff: Vector = sub(rp, pp)
    var prod1: Double = dot(diff, pn)
    var prod2: Double = dot(rv, pn)
    var prod3: Double = prod1 / prod2
    return sub(rp, mul(rv, prod3))
}

fun user_main(): Unit {
    var rv: Vector = Vector(x = 0.0, y = 0.0 - 1.0, z = 0.0 - 1.0)
    var rp: Vector = Vector(x = 0.0, y = 0.0, z = 10.0)
    var pn: Vector = Vector(x = 0.0, y = 0.0, z = 1.0)
    var pp: Vector = Vector(x = 0.0, y = 0.0, z = 5.0)
    var ip: Vector = intersectPoint(rv, rp, pn, pp)
    println(((((("The ray intersects the plane at (" + ip.x.toString()) + ", ") + ip.y.toString()) + ", ") + ip.z.toString()) + ")")
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
