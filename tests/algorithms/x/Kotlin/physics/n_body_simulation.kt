fun <T> _listSet(lst: MutableList<T>, idx: Int, v: T) { while (lst.size <= idx) lst.add(v); lst[idx] = v }

fun json(v: Any?) { println(toJson(v)) }

fun toJson(v: Any?): String = when (v) {
    null -> "null"
    is String -> "\"" + v.replace("\"", "\\\"") + "\""
    is Boolean, is Number -> v.toString()
    is Map<*, *> -> v.entries.joinToString(prefix = "{", postfix = "}") { toJson(it.key.toString()) + ":" + toJson(it.value) }
    is Iterable<*> -> v.joinToString(prefix = "[", postfix = "]") { toJson(it) }
    else -> toJson(v.toString())
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

data class Body(var position_x: Double = 0.0, var position_y: Double = 0.0, var velocity_x: Double = 0.0, var velocity_y: Double = 0.0, var mass: Double = 0.0)
data class BodySystem(var bodies: MutableList<Body> = mutableListOf<Body>(), var gravitation_constant: Double = 0.0, var time_factor: Double = 0.0, var softening_factor: Double = 0.0)
fun make_body(px: Double, py: Double, vx: Double, vy: Double, mass: Double): Body {
    return Body(position_x = px, position_y = py, velocity_x = vx, velocity_y = vy, mass = mass)
}

fun update_velocity(body: Body, force_x: Double, force_y: Double, delta_time: Double): Body {
    body.velocity_x = body.velocity_x + (force_x * delta_time)
    body.velocity_y = body.velocity_y + (force_y * delta_time)
    return body
}

fun update_position(body: Body, delta_time: Double): Body {
    body.position_x = body.position_x + (body.velocity_x * delta_time)
    body.position_y = body.position_y + (body.velocity_y * delta_time)
    return body
}

fun make_body_system(bodies: MutableList<Body>, g: Double, tf: Double, sf: Double): BodySystem {
    return BodySystem(bodies = bodies, gravitation_constant = g, time_factor = tf, softening_factor = sf)
}

fun sqrtApprox(x: Double): Double {
    var guess: Double = x / 2.0
    var i: Int = (0).toInt()
    while (i < 20) {
        guess = (guess + (x / guess)) / 2.0
        i = i + 1
    }
    return guess
}

fun update_system(system: BodySystem, delta_time: Double): BodySystem {
    var bodies: MutableList<Body> = system.bodies
    var i: Int = (0).toInt()
    while (i < bodies.size) {
        var body1: Body = bodies[i]!!
        var force_x: Double = 0.0
        var force_y: Double = 0.0
        var j: Int = (0).toInt()
        while (j < bodies.size) {
            if (i != j) {
                var body2: Body = bodies[j]!!
                var dif_x: Double = body2.position_x - body1.position_x
                var dif_y: Double = body2.position_y - body1.position_y
                var distance_sq: Double = ((dif_x * dif_x) + (dif_y * dif_y)) + system.softening_factor
                var distance: Double = sqrtApprox(distance_sq)
                var denom: Double = (distance * distance) * distance
                force_x = force_x + (((system.gravitation_constant * body2.mass) * dif_x) / denom)
                force_y = force_y + (((system.gravitation_constant * body2.mass) * dif_y) / denom)
            }
            j = j + 1
        }
        body1 = update_velocity(body1, force_x, force_y, delta_time * system.time_factor)
        _listSet(bodies, i, body1)
        i = i + 1
    }
    i = 0
    while (i < bodies.size) {
        var body: Body = bodies[i]!!
        body = update_position(body, delta_time * system.time_factor)
        _listSet(bodies, i, body)
        i = i + 1
    }
    system.bodies = bodies
    return system
}

fun user_main(): Unit {
    var b1: Body = make_body(0.0, 0.0, 0.0, 0.0, 1.0)
    var b2: Body = make_body(10.0, 0.0, 0.0, 0.0, 1.0)
    var sys1: BodySystem = make_body_system(mutableListOf(b1, b2), 1.0, 1.0, 0.0)
    sys1 = update_system(sys1, 1.0)
    var b1_after: Body = (sys1.bodies)[0]!!
    var pos1x: Double = b1_after.position_x
    var pos1y: Double = b1_after.position_y
    json((mutableMapOf<String, Double>("x" to (pos1x), "y" to (pos1y)) as Any?))
    var vel1x: Double = b1_after.velocity_x
    var vel1y: Double = b1_after.velocity_y
    json((mutableMapOf<String, Double>("vx" to (vel1x), "vy" to (vel1y)) as Any?))
    var b3: Body = make_body(0.0 - 10.0, 0.0, 0.0, 0.0, 1.0)
    var b4: Body = make_body(10.0, 0.0, 0.0, 0.0, 4.0)
    var sys2: BodySystem = make_body_system(mutableListOf(b3, b4), 1.0, 10.0, 0.0)
    sys2 = update_system(sys2, 1.0)
    var b2_after: Body = (sys2.bodies)[0]!!
    var pos2x: Double = b2_after.position_x
    var pos2y: Double = b2_after.position_y
    json((mutableMapOf<String, Double>("x" to (pos2x), "y" to (pos2y)) as Any?))
    var vel2x: Double = b2_after.velocity_x
    var vel2y: Double = b2_after.velocity_y
    json((mutableMapOf<String, Double>("vx" to (vel2x), "vy" to (vel2y)) as Any?))
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
