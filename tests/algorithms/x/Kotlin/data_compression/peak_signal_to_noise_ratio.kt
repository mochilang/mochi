fun expect(cond: Boolean) { if (!cond) throw RuntimeException("expect failed") }

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

fun sqrtApprox(x: Double): Double {
    if (x <= 0.0) {
        return 0.0
    }
    var guess: Double = x
    var i: Int = 0
    while (i < 10) {
        guess = (guess + (x / guess)) / 2.0
        i = i + 1
    }
    return guess
}

fun ln(x: Double): Double {
    var t: Double = (x - 1.0) / (x + 1.0)
    var term: Double = t
    var sum: Double = 0.0
    var n: Int = 1
    while (n <= 19) {
        sum = sum + (term / ((n.toDouble())))
        term = (term * t) * t
        n = n + 2
    }
    return 2.0 * sum
}

fun log10(x: Double): Double {
    return ln(x) / ln(10.0)
}

fun peak_signal_to_noise_ratio(original: MutableList<MutableList<Int>>, contrast: MutableList<MutableList<Int>>): Double {
    var mse: Double = 0.0
    var i: Int = 0
    while (i < original.size) {
        var j: Int = 0
        while (j < (original[i]!!).size) {
            var diff: Double = (((((original[i]!!) as MutableList<Int>))[j]!! - (((contrast[i]!!) as MutableList<Int>))[j]!!).toDouble())
            mse = mse + (diff * diff)
            j = j + 1
        }
        i = i + 1
    }
    var size: Double = ((original.size * (original[0]!!).size).toDouble())
    mse = mse / size
    if (mse == 0.0) {
        return 100.0
    }
    var PIXEL_MAX: Double = 255.0
    return 20.0 * log10(PIXEL_MAX / sqrtApprox(mse))
}

fun test_identical_images(): Unit {
    var img: MutableList<MutableList<Int>> = mutableListOf(mutableListOf(52, 55), mutableListOf(61, 59))
    expect(peak_signal_to_noise_ratio(img, img) == 100.0)
}

fun test_single_pixel_difference(): Unit {
    var original: MutableList<MutableList<Int>> = mutableListOf(mutableListOf(0, 0), mutableListOf(0, 0))
    var contrast: MutableList<MutableList<Int>> = mutableListOf(mutableListOf(0, 0), mutableListOf(0, 1))
    var psnr: Double = peak_signal_to_noise_ratio(original, contrast)
    var expected: Double = 20.0 * log10(255.0 / sqrtApprox(0.25))
    expect(kotlin.math.abs(psnr - expected) < 0.0001)
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        test_identical_images()
        test_single_pixel_difference()
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
