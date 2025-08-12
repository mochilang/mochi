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

var xs: MutableList<Double> = mutableListOf(0.0, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0)
var ys: MutableList<Double> = mutableListOf<Double>()
var i: Int = (0).toInt()
var X: MutableList<MutableList<Double>> = design_matrix(xs, 3)
var Xt: MutableList<MutableList<Double>> = transpose(X)
var XtX: MutableList<MutableList<Double>> = matmul(Xt, X)
var Xty: MutableList<Double> = matvec_mul(Xt, ys)
var coeffs: MutableList<Double> = gaussian_elimination(XtX, Xty)
fun design_matrix(xs: MutableList<Double>, degree: Int): MutableList<MutableList<Double>> {
    var i: Int = (0).toInt()
    var matrix: MutableList<MutableList<Double>> = mutableListOf<MutableList<Double>>()
    while (i < xs.size) {
        var row: MutableList<Double> = mutableListOf<Double>()
        var j: Int = (0).toInt()
        var pow: Double = 1.0
        while (j <= degree) {
            row = run { val _tmp = row.toMutableList(); _tmp.add(pow); _tmp }
            pow = pow * xs[i]!!
            j = j + 1
        }
        matrix = run { val _tmp = matrix.toMutableList(); _tmp.add(row); _tmp }
        i = (i + 1).toInt()
    }
    return matrix
}

fun transpose(matrix: MutableList<MutableList<Double>>): MutableList<MutableList<Double>> {
    var rows: Int = (matrix.size).toInt()
    var cols: Int = ((matrix[0]!!).size).toInt()
    var j: Int = (0).toInt()
    var result: MutableList<MutableList<Double>> = mutableListOf<MutableList<Double>>()
    while (j < cols) {
        var row: MutableList<Double> = mutableListOf<Double>()
        var i: Int = (0).toInt()
        while (i < rows) {
            row = run { val _tmp = row.toMutableList(); _tmp.add((((matrix[i]!!) as MutableList<Double>))[j]!!); _tmp }
            i = (i + 1).toInt()
        }
        result = run { val _tmp = result.toMutableList(); _tmp.add(row); _tmp }
        j = j + 1
    }
    return result
}

fun matmul(A: MutableList<MutableList<Double>>, B: MutableList<MutableList<Double>>): MutableList<MutableList<Double>> {
    var n: Int = (A.size).toInt()
    var m: Int = ((A[0]!!).size).toInt()
    var p: Int = ((B[0]!!).size).toInt()
    var i: Int = (0).toInt()
    var result: MutableList<MutableList<Double>> = mutableListOf<MutableList<Double>>()
    while (i < n) {
        var row: MutableList<Double> = mutableListOf<Double>()
        var k: Int = (0).toInt()
        while (k < p) {
            var sum: Double = 0.0
            var j: Int = (0).toInt()
            while (j < m) {
                sum = sum + ((((A[i]!!) as MutableList<Double>))[j]!! * (((B[j]!!) as MutableList<Double>))[k]!!)
                j = j + 1
            }
            row = run { val _tmp = row.toMutableList(); _tmp.add(sum); _tmp }
            k = k + 1
        }
        result = run { val _tmp = result.toMutableList(); _tmp.add(row); _tmp }
        i = (i + 1).toInt()
    }
    return result
}

fun matvec_mul(A: MutableList<MutableList<Double>>, v: MutableList<Double>): MutableList<Double> {
    var n: Int = (A.size).toInt()
    var m: Int = ((A[0]!!).size).toInt()
    var i: Int = (0).toInt()
    var result: MutableList<Double> = mutableListOf<Double>()
    while (i < n) {
        var sum: Double = 0.0
        var j: Int = (0).toInt()
        while (j < m) {
            sum = sum + ((((A[i]!!) as MutableList<Double>))[j]!! * v[j]!!)
            j = j + 1
        }
        result = run { val _tmp = result.toMutableList(); _tmp.add(sum); _tmp }
        i = (i + 1).toInt()
    }
    return result
}

fun gaussian_elimination(A: MutableList<MutableList<Double>>, b: MutableList<Double>): MutableList<Double> {
    var n: Int = (A.size).toInt()
    var M: MutableList<MutableList<Double>> = mutableListOf<MutableList<Double>>()
    var i: Int = (0).toInt()
    while (i < n) {
        M = run { val _tmp = M.toMutableList(); _tmp.add(run { val _tmp = (A[i]!!).toMutableList(); _tmp.add(b[i]!!); _tmp }); _tmp }
        i = (i + 1).toInt()
    }
    var k: Int = (0).toInt()
    while (k < n) {
        var j: BigInteger = ((k + 1).toBigInteger())
        while (j.compareTo((n).toBigInteger()) < 0) {
            var factor: Double = (((M[(j).toInt()]!!) as MutableList<Double>))[k]!! / (((M[k]!!) as MutableList<Double>))[k]!!
            var rowj: MutableList<Double> = M[(j).toInt()]!!
            var rowk: MutableList<Double> = M[k]!!
            var l: Int = (k).toInt()
            while (l <= n) {
                _listSet(rowj, l, rowj[l]!! - (factor * rowk[l]!!))
                l = l + 1
            }
            _listSet(M, (j).toInt(), rowj)
            j = j.add((1).toBigInteger())
        }
        k = k + 1
    }
    var x: MutableList<Double> = mutableListOf<Double>()
    var t: Int = (0).toInt()
    while (t < n) {
        x = run { val _tmp = x.toMutableList(); _tmp.add(0.0); _tmp }
        t = t + 1
    }
    var i2: BigInteger = ((n - 1).toBigInteger())
    while (i2.compareTo((0).toBigInteger()) >= 0) {
        var sum: Double = (((M[(i2).toInt()]!!) as MutableList<Double>))[n]!!
        var j2: BigInteger = i2.add((1).toBigInteger())
        while (j2.compareTo((n).toBigInteger()) < 0) {
            sum = sum - ((((M[(i2).toInt()]!!) as MutableList<Double>))[(j2).toInt()]!! * x[(j2).toInt()]!!)
            j2 = j2.add((1).toBigInteger())
        }
        _listSet(x, (i2).toInt(), sum / (((M[(i2).toInt()]!!) as MutableList<Double>))[(i2).toInt()]!!)
        i2 = i2.subtract((1).toBigInteger())
    }
    return x
}

fun predict(xs: MutableList<Double>, coeffs: MutableList<Double>): MutableList<Double> {
    var i: Int = (0).toInt()
    var result: MutableList<Double> = mutableListOf<Double>()
    while (i < xs.size) {
        var x: Double = xs[i]!!
        var j: Int = (0).toInt()
        var pow: Double = 1.0
        var sum: Double = 0.0
        while (j < coeffs.size) {
            sum = sum + (coeffs[j]!! * pow)
            pow = pow * x
            j = j + 1
        }
        result = run { val _tmp = result.toMutableList(); _tmp.add(sum); _tmp }
        i = (i + 1).toInt()
    }
    return result
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        while (i < xs.size) {
            var x: Double = xs[i]!!
            ys = run { val _tmp = ys.toMutableList(); _tmp.add(((((x * x) * x) - ((2.0 * x) * x)) + (3.0 * x)) - 5.0); _tmp }
            i = (i + 1).toInt()
        }
        println(coeffs.toString())
        println(predict(mutableListOf(0.0 - 1.0), coeffs).toString())
        println(predict(mutableListOf(0.0 - 2.0), coeffs).toString())
        println(predict(mutableListOf(6.0), coeffs).toString())
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
