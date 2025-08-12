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

var PI: Double = 3.141592653589793
var sample_rate: Int = (8000).toInt()
var size: Int = (16).toInt()
var audio: MutableList<Double> = mutableListOf<Double>()
var n: Int = (0).toInt()
var coeffs: MutableList<Double> = mfcc(audio, 5, 3)
fun sinApprox(x: Double): Double {
    var term: Double = x
    var sum: Double = x
    var n: Int = (1).toInt()
    while (n <= 10) {
        var denom: Double = (((2 * n) * ((2 * n) + 1)).toDouble())
        term = (((0.0 - term) * x) * x) / denom
        sum = sum + term
        n = (n + 1).toInt()
    }
    return sum
}

fun cosApprox(x: Double): Double {
    var term: Double = 1.0
    var sum: Double = 1.0
    var n: Int = (1).toInt()
    while (n <= 10) {
        var denom: Double = ((((2 * n) - 1) * (2 * n)).toDouble())
        term = (((0.0 - term) * x) * x) / denom
        sum = sum + term
        n = (n + 1).toInt()
    }
    return sum
}

fun expApprox(x: Double): Double {
    var sum: Double = 1.0
    var term: Double = 1.0
    var n: Int = (1).toInt()
    while (n < 10) {
        term = (term * x) / ((n.toDouble()))
        sum = sum + term
        n = (n + 1).toInt()
    }
    return sum
}

fun ln(x: Double): Double {
    var t: Double = (x - 1.0) / (x + 1.0)
    var term: Double = t
    var sum: Double = 0.0
    var n: Int = (1).toInt()
    while (n <= 19) {
        sum = sum + (term / ((n.toDouble())))
        term = (term * t) * t
        n = (n + 2).toInt()
    }
    return 2.0 * sum
}

fun log10(x: Double): Double {
    return ln(x) / ln(10.0)
}

fun sqrtApprox(x: Double): Double {
    if (x <= 0.0) {
        return 0.0
    }
    var guess: Double = x
    var i: Int = (0).toInt()
    while (i < 10) {
        guess = (guess + (x / guess)) / 2.0
        i = i + 1
    }
    return guess
}

fun absf(x: Double): Double {
    if (x < 0.0) {
        return 0.0 - x
    }
    return x
}

fun normalize(audio: MutableList<Double>): MutableList<Double> {
    var max_val: Double = 0.0
    var i: Int = (0).toInt()
    while (i < audio.size) {
        var v: Double = absf(audio[i]!!)
        if (v > max_val) {
            max_val = v
        }
        i = i + 1
    }
    var res: MutableList<Double> = mutableListOf<Double>()
    i = 0
    while (i < audio.size) {
        res = run { val _tmp = res.toMutableList(); _tmp.add(audio[i]!! / max_val); _tmp }
        i = i + 1
    }
    return res
}

fun dft(frame: MutableList<Double>, bins: Int): MutableList<Double> {
    var N: Int = (frame.size).toInt()
    var spec: MutableList<Double> = mutableListOf<Double>()
    var k: Int = (0).toInt()
    while (k < bins) {
        var real: Double = 0.0
        var imag: Double = 0.0
        var n: Int = (0).toInt()
        while (n < N) {
            var angle: Double = ((((0.0 - 2.0) * PI) * ((k.toDouble()))) * ((n.toDouble()))) / ((N.toDouble()))
            real = real + (frame[n]!! * cosApprox(angle))
            imag = imag + (frame[n]!! * sinApprox(angle))
            n = (n + 1).toInt()
        }
        spec = run { val _tmp = spec.toMutableList(); _tmp.add((real * real) + (imag * imag)); _tmp }
        k = k + 1
    }
    return spec
}

fun triangular_filters(bins: Int, spectrum_size: Int): MutableList<MutableList<Double>> {
    var filters: MutableList<MutableList<Double>> = mutableListOf<MutableList<Double>>()
    var b: Int = (0).toInt()
    while (b < bins) {
        var center: Int = (((b + 1) * spectrum_size) / (bins + 1)).toInt()
        var filt: MutableList<Double> = mutableListOf<Double>()
        var i: Int = (0).toInt()
        while (i < spectrum_size) {
            var v: Double = 0.0
            if (i <= center) {
                v = ((i.toDouble())) / ((center.toDouble()))
            } else {
                v = (((spectrum_size - i).toDouble())) / (((spectrum_size - center).toDouble()))
            }
            filt = run { val _tmp = filt.toMutableList(); _tmp.add(v); _tmp }
            i = i + 1
        }
        filters = run { val _tmp = filters.toMutableList(); _tmp.add(filt); _tmp }
        b = b + 1
    }
    return filters
}

fun dot(mat: MutableList<MutableList<Double>>, vec: MutableList<Double>): MutableList<Double> {
    var res: MutableList<Double> = mutableListOf<Double>()
    var i: Int = (0).toInt()
    while (i < mat.size) {
        var sum: Double = 0.0
        var j: Int = (0).toInt()
        while (j < vec.size) {
            sum = sum + ((((mat[i]!!) as MutableList<Double>))[j]!! * vec[j]!!)
            j = j + 1
        }
        res = run { val _tmp = res.toMutableList(); _tmp.add(sum); _tmp }
        i = i + 1
    }
    return res
}

fun discrete_cosine_transform(dct_filter_num: Int, filter_num: Int): MutableList<MutableList<Double>> {
    var basis: MutableList<MutableList<Double>> = mutableListOf<MutableList<Double>>()
    var i: Int = (0).toInt()
    while (i < dct_filter_num) {
        var row: MutableList<Double> = mutableListOf<Double>()
        var j: Int = (0).toInt()
        while (j < filter_num) {
            if (i == 0) {
                row = run { val _tmp = row.toMutableList(); _tmp.add(1.0 / sqrtApprox((filter_num.toDouble()))); _tmp }
            } else {
                var angle: Double = ((((((2 * j) + 1).toDouble())) * ((i.toDouble()))) * PI) / (2.0 * ((filter_num.toDouble())))
                row = run { val _tmp = row.toMutableList(); _tmp.add(cosApprox(angle) * sqrtApprox(2.0 / ((filter_num.toDouble())))); _tmp }
            }
            j = j + 1
        }
        basis = run { val _tmp = basis.toMutableList(); _tmp.add(row); _tmp }
        i = i + 1
    }
    return basis
}

fun mfcc(audio: MutableList<Double>, bins: Int, dct_num: Int): MutableList<Double> {
    var norm: MutableList<Double> = normalize(audio)
    var spec: MutableList<Double> = dft(norm, bins + 2)
    var filters: MutableList<MutableList<Double>> = triangular_filters(bins, spec.size)
    var energies: MutableList<Double> = dot(filters, spec)
    var logfb: MutableList<Double> = mutableListOf<Double>()
    var i: Int = (0).toInt()
    while (i < energies.size) {
        logfb = run { val _tmp = logfb.toMutableList(); _tmp.add(10.0 * log10(energies[i]!! + 0.0000000001)); _tmp }
        i = i + 1
    }
    var dct_basis: MutableList<MutableList<Double>> = discrete_cosine_transform(dct_num, bins)
    var res: MutableList<Double> = dot(dct_basis, logfb)
    if (res.size == 0) {
        res = mutableListOf(0.0, 0.0, 0.0)
    }
    return res
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        while (n < size) {
            var t: Double = ((n.toDouble())) / ((sample_rate.toDouble()))
            audio = run { val _tmp = audio.toMutableList(); _tmp.add(sinApprox(((2.0 * PI) * 440.0) * t)); _tmp }
            n = (n + 1).toInt()
        }
        for (c in coeffs) {
            println(c)
        }
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
