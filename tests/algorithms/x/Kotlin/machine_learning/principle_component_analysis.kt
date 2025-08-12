fun <T> _listSet(lst: MutableList<T>, idx: Int, v: T) { while (lst.size <= idx) lst.add(v); lst[idx] = v }

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

data class PCAResult(var transformed: MutableList<MutableList<Double>> = mutableListOf<MutableList<Double>>(), var variance_ratio: MutableList<Double> = mutableListOf<Double>())
data class Eigen(var values: MutableList<Double> = mutableListOf<Double>(), var vectors: MutableList<MutableList<Double>> = mutableListOf<MutableList<Double>>())
var data: MutableList<MutableList<Double>> = mutableListOf(mutableListOf(2.5, 2.4), mutableListOf(0.5, 0.7), mutableListOf(2.2, 2.9), mutableListOf(1.9, 2.2), mutableListOf(3.1, 3.0), mutableListOf(2.3, 2.7), mutableListOf(2.0, 1.6), mutableListOf(1.0, 1.1), mutableListOf(1.5, 1.6), mutableListOf(1.1, 0.9))
var result: PCAResult = apply_pca(data, 2)
fun sqrt(x: Double): Double {
    var guess: Double = (if (x > 1.0) x / 2.0 else 1.0.toDouble())
    var i: Int = (0).toInt()
    while (i < 20) {
        guess = 0.5 * (guess + (x / guess))
        i = i + 1
    }
    return guess
}

fun mean(xs: MutableList<Double>): Double {
    var sum: Double = 0.0
    var i: Int = (0).toInt()
    while (i < xs.size) {
        sum = sum + xs[i]!!
        i = i + 1
    }
    return sum / xs.size
}

fun standardize(data: MutableList<MutableList<Double>>): MutableList<MutableList<Double>> {
    var n_samples: Int = (data.size).toInt()
    var n_features: Int = ((data[0]!!).size).toInt()
    var means: MutableList<Double> = mutableListOf<Double>()
    var stds: MutableList<Double> = mutableListOf<Double>()
    var j: Int = (0).toInt()
    while (j < n_features) {
        var column: MutableList<Double> = mutableListOf<Double>()
        var i: Int = (0).toInt()
        while (i < n_samples) {
            column = run { val _tmp = column.toMutableList(); _tmp.add((((data[i]!!) as MutableList<Double>))[j]!!); _tmp }
            i = i + 1
        }
        var m: Double = mean(column)
        means = run { val _tmp = means.toMutableList(); _tmp.add(m); _tmp }
        var variance: Double = 0.0
        var k: Int = (0).toInt()
        while (k < n_samples) {
            var diff: Double = column[k]!! - m
            variance = variance + (diff * diff)
            k = k + 1
        }
        stds = run { val _tmp = stds.toMutableList(); _tmp.add(sqrt(variance / (n_samples - 1))); _tmp }
        j = j + 1
    }
    var standardized: MutableList<MutableList<Double>> = mutableListOf<MutableList<Double>>()
    var r: Int = (0).toInt()
    while (r < n_samples) {
        var row: MutableList<Double> = mutableListOf<Double>()
        var c: Int = (0).toInt()
        while (c < n_features) {
            row = run { val _tmp = row.toMutableList(); _tmp.add(((((data[r]!!) as MutableList<Double>))[c]!! - means[c]!!) / stds[c]!!); _tmp }
            c = c + 1
        }
        standardized = run { val _tmp = standardized.toMutableList(); _tmp.add(row); _tmp }
        r = r + 1
    }
    return standardized
}

fun covariance_matrix(data: MutableList<MutableList<Double>>): MutableList<MutableList<Double>> {
    var n_samples: Int = (data.size).toInt()
    var n_features: Int = ((data[0]!!).size).toInt()
    var cov: MutableList<MutableList<Double>> = mutableListOf<MutableList<Double>>()
    var i: Int = (0).toInt()
    while (i < n_features) {
        var row: MutableList<Double> = mutableListOf<Double>()
        var j: Int = (0).toInt()
        while (j < n_features) {
            var sum: Double = 0.0
            var k: Int = (0).toInt()
            while (k < n_samples) {
                sum = sum + ((((data[k]!!) as MutableList<Double>))[i]!! * (((data[k]!!) as MutableList<Double>))[j]!!)
                k = k + 1
            }
            row = run { val _tmp = row.toMutableList(); _tmp.add(sum / (n_samples - 1)); _tmp }
            j = j + 1
        }
        cov = run { val _tmp = cov.toMutableList(); _tmp.add(row); _tmp }
        i = i + 1
    }
    return cov
}

fun normalize(vec: MutableList<Double>): MutableList<Double> {
    var sum: Double = 0.0
    var i: Int = (0).toInt()
    while (i < vec.size) {
        sum = sum + (vec[i]!! * vec[i]!!)
        i = i + 1
    }
    var n: Double = sqrt(sum)
    var res: MutableList<Double> = mutableListOf<Double>()
    var j: Int = (0).toInt()
    while (j < vec.size) {
        res = run { val _tmp = res.toMutableList(); _tmp.add(vec[j]!! / n); _tmp }
        j = j + 1
    }
    return res
}

fun eigen_decomposition_2x2(matrix: MutableList<MutableList<Double>>): Eigen {
    var a: Double = (((matrix[0]!!) as MutableList<Double>))[0]!!
    var b: Double = (((matrix[0]!!) as MutableList<Double>))[1]!!
    var c: Double = (((matrix[1]!!) as MutableList<Double>))[1]!!
    var diff: Double = a - c
    var discriminant: Double = sqrt((diff * diff) + ((4.0 * b) * b))
    var lambda1: Double = ((a + c) + discriminant) / 2.0
    var lambda2: Double = ((a + c) - discriminant) / 2.0
    var v1: MutableList<Double> = mutableListOf<Double>()
    var v2: MutableList<Double> = mutableListOf<Double>()
    if (b != 0.0) {
        v1 = normalize(mutableListOf(lambda1 - c, b))
        v2 = normalize(mutableListOf(lambda2 - c, b))
    } else {
        v1 = mutableListOf(1.0, 0.0)
        v2 = mutableListOf(0.0, 1.0)
    }
    var eigenvalues: MutableList<Double> = mutableListOf(lambda1, lambda2)
    var eigenvectors: MutableList<MutableList<Double>> = mutableListOf(v1, v2)
    if (eigenvalues[0]!! < eigenvalues[1]!!) {
        var tmp_val: Double = eigenvalues[0]!!
        _listSet(eigenvalues, 0, eigenvalues[1]!!)
        _listSet(eigenvalues, 1, tmp_val)
        var tmp_vec: MutableList<Double> = eigenvectors[0]!!
        _listSet(eigenvectors, 0, eigenvectors[1]!!)
        _listSet(eigenvectors, 1, tmp_vec)
    }
    return Eigen(values = eigenvalues, vectors = eigenvectors)
}

fun transpose(matrix: MutableList<MutableList<Double>>): MutableList<MutableList<Double>> {
    var rows: Int = (matrix.size).toInt()
    var cols: Int = ((matrix[0]!!).size).toInt()
    var trans: MutableList<MutableList<Double>> = mutableListOf<MutableList<Double>>()
    var i: Int = (0).toInt()
    while (i < cols) {
        var row: MutableList<Double> = mutableListOf<Double>()
        var j: Int = (0).toInt()
        while (j < rows) {
            row = run { val _tmp = row.toMutableList(); _tmp.add((((matrix[j]!!) as MutableList<Double>))[i]!!); _tmp }
            j = j + 1
        }
        trans = run { val _tmp = trans.toMutableList(); _tmp.add(row); _tmp }
        i = i + 1
    }
    return trans
}

fun matrix_multiply(a: MutableList<MutableList<Double>>, b: MutableList<MutableList<Double>>): MutableList<MutableList<Double>> {
    var rows_a: Int = (a.size).toInt()
    var cols_a: Int = ((a[0]!!).size).toInt()
    var rows_b: Int = (b.size).toInt()
    var cols_b: Int = ((b[0]!!).size).toInt()
    if (cols_a != rows_b) {
        panic("Incompatible matrices")
    }
    var result: MutableList<MutableList<Double>> = mutableListOf<MutableList<Double>>()
    var i: Int = (0).toInt()
    while (i < rows_a) {
        var row: MutableList<Double> = mutableListOf<Double>()
        var j: Int = (0).toInt()
        while (j < cols_b) {
            var sum: Double = 0.0
            var k: Int = (0).toInt()
            while (k < cols_a) {
                sum = sum + ((((a[i]!!) as MutableList<Double>))[k]!! * (((b[k]!!) as MutableList<Double>))[j]!!)
                k = k + 1
            }
            row = run { val _tmp = row.toMutableList(); _tmp.add(sum); _tmp }
            j = j + 1
        }
        result = run { val _tmp = result.toMutableList(); _tmp.add(row); _tmp }
        i = i + 1
    }
    return result
}

fun apply_pca(data: MutableList<MutableList<Double>>, n_components: Int): PCAResult {
    var standardized: MutableList<MutableList<Double>> = standardize(data)
    var cov: MutableList<MutableList<Double>> = covariance_matrix(standardized)
    var eig: Eigen = eigen_decomposition_2x2(cov)
    var eigenvalues: MutableList<Double> = eig.values
    var eigenvectors: MutableList<MutableList<Double>> = eig.vectors
    var components: MutableList<MutableList<Double>> = transpose(eigenvectors)
    var transformed: MutableList<MutableList<Double>> = matrix_multiply(standardized, components)
    var total: Double = eigenvalues[0]!! + eigenvalues[1]!!
    var ratios: MutableList<Double> = mutableListOf<Double>()
    var i: Int = (0).toInt()
    while (i < n_components) {
        ratios = run { val _tmp = ratios.toMutableList(); _tmp.add(eigenvalues[i]!! / total); _tmp }
        i = i + 1
    }
    return PCAResult(transformed = transformed, variance_ratio = ratios)
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println("Transformed Data (first 5 rows):")
        var idx: Int = (0).toInt()
        while (idx < 5) {
            println((result.transformed)[idx]!!)
            idx = (idx + 1).toInt()
        }
        println("Explained Variance Ratio:")
        println(result.variance_ratio)
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
