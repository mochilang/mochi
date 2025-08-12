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

fun doppler_effect(org_freq: Double, wave_vel: Double, obs_vel: Double, src_vel: Double): Double {
    if (wave_vel == src_vel) {
        panic("division by zero implies vs=v and observer in front of the source")
    }
    var doppler_freq: Double = (org_freq * (wave_vel + obs_vel)) / (wave_vel - src_vel)
    if (doppler_freq <= 0.0) {
        panic("non-positive frequency implies vs>v or v0>v (in the opposite direction)")
    }
    return doppler_freq
}

fun absf(x: Double): Double {
    if (x < 0.0) {
        return 0.0 - x
    }
    return x
}

fun almost_equal(a: Double, b: Double, tol: Double): Boolean {
    return absf(a - b) <= tol
}

fun test_doppler_effect(): Unit {
    if (!almost_equal(doppler_effect(100.0, 330.0, 10.0, 0.0), 103.03030303030303, 0.0000001)) {
        panic("test 1 failed")
    }
    if (!almost_equal(doppler_effect(100.0, 330.0, 0.0 - 10.0, 0.0), 96.96969696969697, 0.0000001)) {
        panic("test 2 failed")
    }
    if (!almost_equal(doppler_effect(100.0, 330.0, 0.0, 10.0), 103.125, 0.0000001)) {
        panic("test 3 failed")
    }
    if (!almost_equal(doppler_effect(100.0, 330.0, 0.0, 0.0 - 10.0), 97.05882352941177, 0.0000001)) {
        panic("test 4 failed")
    }
    if (!almost_equal(doppler_effect(100.0, 330.0, 10.0, 10.0), 106.25, 0.0000001)) {
        panic("test 5 failed")
    }
    if (!almost_equal(doppler_effect(100.0, 330.0, 0.0 - 10.0, 0.0 - 10.0), 94.11764705882354, 0.0000001)) {
        panic("test 6 failed")
    }
}

fun user_main(): Unit {
    test_doppler_effect()
    println(doppler_effect(100.0, 330.0, 10.0, 0.0))
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
