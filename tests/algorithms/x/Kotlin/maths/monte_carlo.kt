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

var PI: Double = 3.141592653589793
var rand_seed: Int = (123456789).toInt()
fun rand_float(): Double {
    rand_seed = ((Math.floorMod((((1103515245 * rand_seed) + 12345).toLong()), 2147483648L)).toInt()).toInt()
    return (rand_seed.toDouble()) / 2147483648.0
}

fun rand_range(min_val: Double, max_val: Double): Double {
    return (rand_float() * (max_val - min_val)) + min_val
}

fun abs_float(x: Double): Double {
    if (x < 0.0) {
        return 0.0 - x
    }
    return x
}

fun sqrtApprox(x: Double): Double {
    if (x == 0.0) {
        return 0.0
    }
    var guess: Double = x / 2.0
    var i: Int = (0).toInt()
    while (i < 20) {
        guess = (guess + (x / guess)) / 2.0
        i = i + 1
    }
    return guess
}

fun pi_estimator(iterations: Int): Unit {
    var inside: Double = 0.0
    var i: Int = (0).toInt()
    while (i < iterations) {
        var x: Double = rand_range(0.0 - 1.0, 1.0)
        var y: Double = rand_range(0.0 - 1.0, 1.0)
        if (((x * x) + (y * y)) <= 1.0) {
            inside = inside + 1.0
        }
        i = i + 1
    }
    var proportion: Double = inside / (iterations.toDouble())
    var pi_estimate: Double = proportion * 4.0
    println(listOf("The estimated value of pi is", pi_estimate).joinToString(" "))
    println(listOf("The numpy value of pi is", PI).joinToString(" "))
    println(listOf("The total error is", abs_float(PI - pi_estimate)).joinToString(" "))
}

fun area_under_curve_estimator(iterations: Int, f: (Double) -> Double, min_value: Double, max_value: Double): Double {
    var sum: Double = 0.0
    var i: Int = (0).toInt()
    while (i < iterations) {
        var x: Double = rand_range(min_value, max_value)
        sum = sum + f(x)
        i = i + 1
    }
    var expected: Double = sum / (iterations.toDouble())
    return expected * (max_value - min_value)
}

fun area_under_line_estimator_check(iterations: Int, min_value: Double, max_value: Double): Unit {
    fun identity_function(x: Double): Double {
        return x
    }

    var estimated_value: Double = area_under_curve_estimator(iterations, ::identity_function, min_value, max_value)
    var expected_value: Double = (((max_value * max_value) - (min_value * min_value)).toDouble()) / 2.0
    println("******************")
    println(listOf("Estimating area under y=x where x varies from", min_value).joinToString(" "))
    println(listOf("Estimated value is", estimated_value).joinToString(" "))
    println(listOf("Expected value is", expected_value).joinToString(" "))
    println(listOf("Total error is", abs_float(estimated_value - expected_value)).joinToString(" "))
    println("******************")
}

fun pi_estimator_using_area_under_curve(iterations: Int): Unit {
    fun semi_circle(x: Double): Double {
        var y: Double = (4.0 - ((x * x).toDouble())).toDouble()
        var s: Double = sqrtApprox(y)
        return s
    }

    var estimated_value: Double = area_under_curve_estimator(iterations, ::semi_circle, 0.0, 2.0)
    println("******************")
    println("Estimating pi using area_under_curve_estimator")
    println(listOf("Estimated value is", estimated_value).joinToString(" "))
    println(listOf("Expected value is", PI).joinToString(" "))
    println(listOf("Total error is", abs_float(estimated_value - PI)).joinToString(" "))
    println("******************")
}

fun user_main(): Unit {
    pi_estimator(1000)
    area_under_line_estimator_check(1000, 0.0, 1.0)
    pi_estimator_using_area_under_curve(1000)
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
