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

data class Point(var x: Double = 0.0, var y: Double = 0.0)
var PI: Double = 3.141592653589793
var seed: Int = (1).toInt()
fun next_seed(x: Int): Int {
    return (Math.floorMod((((x * 1103515245) + 12345).toLong()), 2147483648L)).toInt()
}

fun rand_unit(): Double {
    seed = (next_seed(seed)).toInt()
    return (seed.toDouble()) / 2147483648.0
}

fun is_in_unit_circle(p: Point): Boolean {
    return ((p.x * p.x) + (p.y * p.y)) <= 1.0
}

fun random_unit_square(): Point {
    return Point(x = rand_unit(), y = rand_unit())
}

fun estimate_pi(simulations: Int): Double {
    if (simulations < 1) {
        panic("At least one simulation is necessary to estimate PI.")
    }
    var inside: Int = (0).toInt()
    var i: Int = (0).toInt()
    while (i < simulations) {
        var p: Point = random_unit_square()
        if ((is_in_unit_circle(p)) as Boolean) {
            inside = inside + 1
        }
        i = i + 1
    }
    return (4.0 * (inside.toDouble())) / (simulations.toDouble())
}

fun abs_float(x: Double): Double {
    if (x < 0.0) {
        return 0.0 - x
    }
    return x
}

fun user_main(): Unit {
    var n: Int = (10000).toInt()
    var my_pi: Double = estimate_pi(n)
    var error: Double = abs_float(my_pi - PI)
    println((("An estimate of PI is " + _numToStr(my_pi)) + " with an error of ") + _numToStr(error))
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
