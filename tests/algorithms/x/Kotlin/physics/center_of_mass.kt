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

data class Particle(var x: Double = 0.0, var y: Double = 0.0, var z: Double = 0.0, var mass: Double = 0.0)
data class Coord3D(var x: Double = 0.0, var y: Double = 0.0, var z: Double = 0.0)
var r1: Coord3D = center_of_mass(mutableListOf(Particle(x = 1.5, y = 4.0, z = 3.4, mass = 4.0), Particle(x = 5.0, y = 6.8, z = 7.0, mass = 8.1), Particle(x = 9.4, y = 10.1, z = 11.6, mass = 12.0)))
fun round2(x: Double): Double {
    var scaled: Double = x * 100.0
    var rounded: Double = ((((scaled + 0.5).toInt())).toDouble())
    return rounded / 100.0
}

fun center_of_mass(ps: MutableList<Particle>): Coord3D {
    if (ps.size == 0) {
        panic("No particles provided")
    }
    var i: Int = (0).toInt()
    var total_mass: Double = 0.0
    while (i < ps.size) {
        var p: Particle = ps[i]!!
        if (p.mass <= 0.0) {
            panic("Mass of all particles must be greater than 0")
        }
        total_mass = total_mass + p.mass
        i = i + 1
    }
    var sum_x: Double = 0.0
    var sum_y: Double = 0.0
    var sum_z: Double = 0.0
    i = 0
    while (i < ps.size) {
        var p: Particle = ps[i]!!
        sum_x = sum_x + (p.x * p.mass)
        sum_y = sum_y + (p.y * p.mass)
        sum_z = sum_z + (p.z * p.mass)
        i = i + 1
    }
    var cm_x: Double = round2(sum_x / total_mass)
    var cm_y: Double = round2(sum_y / total_mass)
    var cm_z: Double = round2(sum_z / total_mass)
    return Coord3D(x = cm_x, y = cm_y, z = cm_z)
}

fun coord_to_string(c: Coord3D): String {
    return ((((("Coord3D(x=" + c.x.toString()) + ", y=") + c.y.toString()) + ", z=") + c.z.toString()) + ")"
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println(coord_to_string(r1))
        var r2: Coord3D = center_of_mass(mutableListOf(Particle(x = 1.0, y = 2.0, z = 3.0, mass = 4.0), Particle(x = 5.0, y = 6.0, z = 7.0, mass = 8.0), Particle(x = 9.0, y = 10.0, z = 11.0, mass = 12.0)))
        println(coord_to_string(r2))
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
