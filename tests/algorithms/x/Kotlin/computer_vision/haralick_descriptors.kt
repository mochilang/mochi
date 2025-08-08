import java.math.BigInteger

fun <T> _listSet(lst: MutableList<T>, idx: Int, v: T) { while (lst.size <= idx) lst.add(v); lst[idx] = v }

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

var image: MutableList<MutableList<Int>> = mutableListOf(mutableListOf(0, 1, 0), mutableListOf(1, 0, 1), mutableListOf(0, 1, 0))
var glcm: MutableList<MutableList<Double>> = matrix_concurrency(image, mutableListOf(0, 1))
var descriptors: MutableList<Double> = haralick_descriptors(glcm)
var idx: Int = 0
fun abs_int(n: Int): Int {
    if (n < 0) {
        return 0 - n
    }
    return n
}

fun sqrt(x: Double): Double {
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
    if (x <= 0.0) {
        return 0.0
    }
    var e: Double = 2.718281828
    var n: Int = 0
    var y: Double = x
    while (y >= e) {
        y = y / e
        n = n + 1
    }
    while (y <= (1.0 / e)) {
        y = y * e
        n = n - 1
    }
    y = y - 1.0
    var term: Double = y
    var result: Double = 0.0
    var k: Int = 1
    while (k <= 20) {
        if ((Math.floorMod(k, 2)) == 1) {
            result = result + (term / (1.0 * k))
        } else {
            result = result - (term / (1.0 * k))
        }
        term = term * y
        k = k + 1
    }
    return result + (1.0 * n)
}

fun matrix_concurrency(image: MutableList<MutableList<Int>>, coord: MutableList<Int>): MutableList<MutableList<Double>> {
    var offset_x: Int = coord[0]!!
    var offset_y: Int = coord[1]!!
    var max_val: Int = 0
    for (r in 0 until image.size) {
        for (c in 0 until (image[r]!!).size) {
            if ((((image[r]!!) as MutableList<Int>))[c]!! > max_val) {
                max_val = (((image[r]!!) as MutableList<Int>))[c]!!
            }
        }
    }
    var size: Int = max_val + 1
    var matrix: MutableList<MutableList<Double>> = mutableListOf<MutableList<Double>>()
    for (i in 0 until size) {
        var row: MutableList<Double> = mutableListOf<Double>()
        for (j in 0 until size) {
            row = run { val _tmp = row.toMutableList(); _tmp.add(0.0); _tmp }
        }
        matrix = run { val _tmp = matrix.toMutableList(); _tmp.add(row); _tmp }
    }
    for (x in 1 until image.size - 1) {
        for (y in 1 until (image[x]!!).size - 1) {
            var base: Int = (((image[x]!!) as MutableList<Int>))[y]!!
            var offset: Int = (((image[x + offset_x]!!) as MutableList<Int>))[y + offset_y]!!
            _listSet(matrix[base]!!, offset, (((matrix[base]!!) as MutableList<Double>))[offset]!! + 1.0)
        }
    }
    var total: Double = 0.0
    for (i in 0 until size) {
        for (j in 0 until size) {
            total = total + (((matrix[i]!!) as MutableList<Double>))[j]!!
        }
    }
    if (total == 0.0) {
        return matrix
    }
    for (i in 0 until size) {
        for (j in 0 until size) {
            _listSet(matrix[i]!!, j, (((matrix[i]!!) as MutableList<Double>))[j]!! / total)
        }
    }
    return matrix
}

fun haralick_descriptors(matrix: MutableList<MutableList<Double>>): MutableList<Double> {
    var rows: Int = matrix.size
    var cols: Int = (matrix[0]!!).size
    var maximum_prob: Double = 0.0
    var correlation: Double = 0.0
    var energy: Double = 0.0
    var contrast: Double = 0.0
    var dissimilarity: Double = 0.0
    var inverse_difference: Double = 0.0
    var homogeneity: Double = 0.0
    var entropy: Double = 0.0
    var i: Int = 0
    while (i < rows) {
        var j: Int = 0
        while (j < cols) {
            var _val: Double = (((matrix[i]!!) as MutableList<Double>))[j]!!
            if (_val > maximum_prob) {
                maximum_prob = _val
            }
            correlation = correlation + (((1.0 * i) * j) * _val)
            energy = energy + (_val * _val)
            var diff: Int = i - j
            var adiff: Int = abs_int(diff)
            contrast = contrast + (_val * ((1.0 * diff) * diff))
            dissimilarity = dissimilarity + (_val * (1.0 * adiff))
            inverse_difference = inverse_difference + (_val / (1.0 + (1.0 * adiff)))
            homogeneity = homogeneity + (_val / (1.0 + ((1.0 * diff) * diff)))
            if (_val > 0.0) {
                entropy = entropy - (_val * ln(_val))
            }
            j = j + 1
        }
        i = i + 1
    }
    return mutableListOf(maximum_prob, correlation, energy, contrast, dissimilarity, inverse_difference, homogeneity, entropy)
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        while (idx < descriptors.size) {
            println((descriptors[idx]!!).toString())
            idx = idx + 1
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
