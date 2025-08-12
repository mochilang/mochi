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

fun abs(x: Double): Double {
    if (x < 0.0) {
        return 0.0 - x
    }
    return x
}

fun pow_int(base: Double, exp: Int): Double {
    var result: Double = 1.0
    var i: Int = (0).toInt()
    while (i < exp) {
        result = result * base
        i = i + 1
    }
    return result
}

fun nth_root(x: Double, n: Int): Double {
    if (x == 0.0) {
        return 0.0
    }
    var guess: Double = x
    var i: Int = (0).toInt()
    while (i < 10) {
        var denom: Double = pow_int(guess, n - 1)
        guess = (((((n - 1).toDouble())) * guess) + (x / denom)) / ((n.toDouble()))
        i = i + 1
    }
    return guess
}

fun round_nearest(x: Double): Double {
    if (x >= 0.0) {
        var n: Int = (((x + 0.5).toInt())).toInt()
        return (n.toDouble())
    }
    var n: Int = (((x - 0.5).toInt())).toInt()
    return (n.toDouble())
}

fun compute_geometric_mean(nums: MutableList<Double>): Double {
    if (nums.size == 0) {
        panic("no numbers")
    }
    var product: Double = 1.0
    var i: Int = (0).toInt()
    while (i < nums.size) {
        product = product * nums[i]!!
        i = i + 1
    }
    if ((product < 0.0) && ((Math.floorMod(nums.size, 2)) == 0)) {
        panic("Cannot Compute Geometric Mean for these numbers.")
    }
    var mean: Double = nth_root(kotlin.math.abs(product), nums.size)
    if (product < 0.0) {
        mean = 0.0 - mean
    }
    var possible: Double = round_nearest(mean)
    if (pow_int(possible, nums.size) == product) {
        mean = possible
    }
    return mean
}

fun test_compute_geometric_mean(): Unit {
    var eps: Double = 0.0001
    var m1: Double = compute_geometric_mean(mutableListOf(2.0, 8.0))
    if (kotlin.math.abs(m1 - 4.0) > eps) {
        panic("test1 failed")
    }
    var m2: Double = compute_geometric_mean(mutableListOf(5.0, 125.0))
    if (kotlin.math.abs(m2 - 25.0) > eps) {
        panic("test2 failed")
    }
    var m3: Double = compute_geometric_mean(mutableListOf(1.0, 0.0))
    if (kotlin.math.abs(m3 - 0.0) > eps) {
        panic("test3 failed")
    }
    var m4: Double = compute_geometric_mean(mutableListOf(1.0, 5.0, 25.0, 5.0))
    if (kotlin.math.abs(m4 - 5.0) > eps) {
        panic("test4 failed")
    }
    var m5: Double = compute_geometric_mean(mutableListOf(0.0 - 5.0, 25.0, 1.0))
    if (kotlin.math.abs(m5 + 5.0) > eps) {
        panic("test5 failed")
    }
}

fun user_main(): Unit {
    test_compute_geometric_mean()
    println(compute_geometric_mean(mutableListOf(0.0 - 3.0, 0.0 - 27.0)))
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
