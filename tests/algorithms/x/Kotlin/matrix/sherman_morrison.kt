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

data class Matrix(var data: MutableList<MutableList<Double>> = mutableListOf<MutableList<Double>>(), var rows: Int = 0, var cols: Int = 0)
fun make_matrix(rows: Int, cols: Int, value: Double): Matrix {
    var arr: MutableList<MutableList<Double>> = mutableListOf<MutableList<Double>>()
    var r: Int = (0).toInt()
    while (r < rows) {
        var row: MutableList<Double> = mutableListOf<Double>()
        var c: Int = (0).toInt()
        while (c < cols) {
            row = run { val _tmp = row.toMutableList(); _tmp.add(value); _tmp }
            c = c + 1
        }
        arr = run { val _tmp = arr.toMutableList(); _tmp.add(row); _tmp }
        r = r + 1
    }
    return Matrix(data = arr, rows = rows, cols = cols)
}

fun matrix_from_lists(vals: MutableList<MutableList<Double>>): Matrix {
    var r: Int = (vals.size).toInt()
    var c: Int = (if (r == 0) 0 else (vals[0]!!).size).toInt()
    return Matrix(data = vals, rows = r, cols = c)
}

fun matrix_to_string(m: Matrix): String {
    var s: String = ""
    var i: Int = (0).toInt()
    while (i < m.rows) {
        s = s + "["
        var j: Int = (0).toInt()
        while (j < m.cols) {
            s = s + (((((m.data)[i]!!) as MutableList<Double>))[j]!!).toString()
            if (j < (m.cols - 1)) {
                s = s + ", "
            }
            j = j + 1
        }
        s = s + "]"
        if (i < (m.rows - 1)) {
            s = s + "\n"
        }
        i = i + 1
    }
    return s
}

fun matrix_add(a: Matrix, b: Matrix): Matrix {
    if ((a.rows != b.rows) || (a.cols != b.cols)) {
        return Matrix(data = mutableListOf<MutableList<Double>>(), rows = 0, cols = 0)
    }
    var res: MutableList<MutableList<Double>> = mutableListOf<MutableList<Double>>()
    var i: Int = (0).toInt()
    while (i < a.rows) {
        var row: MutableList<Double> = mutableListOf<Double>()
        var j: Int = (0).toInt()
        while (j < a.cols) {
            row = run { val _tmp = row.toMutableList(); _tmp.add(((((a.data)[i]!!) as MutableList<Double>))[j]!! + ((((b.data)[i]!!) as MutableList<Double>))[j]!!); _tmp }
            j = j + 1
        }
        res = run { val _tmp = res.toMutableList(); _tmp.add(row); _tmp }
        i = i + 1
    }
    return Matrix(data = res, rows = a.rows, cols = a.cols)
}

fun matrix_sub(a: Matrix, b: Matrix): Matrix {
    if ((a.rows != b.rows) || (a.cols != b.cols)) {
        return Matrix(data = mutableListOf<MutableList<Double>>(), rows = 0, cols = 0)
    }
    var res: MutableList<MutableList<Double>> = mutableListOf<MutableList<Double>>()
    var i: Int = (0).toInt()
    while (i < a.rows) {
        var row: MutableList<Double> = mutableListOf<Double>()
        var j: Int = (0).toInt()
        while (j < a.cols) {
            row = run { val _tmp = row.toMutableList(); _tmp.add(((((a.data)[i]!!) as MutableList<Double>))[j]!! - ((((b.data)[i]!!) as MutableList<Double>))[j]!!); _tmp }
            j = j + 1
        }
        res = run { val _tmp = res.toMutableList(); _tmp.add(row); _tmp }
        i = i + 1
    }
    return Matrix(data = res, rows = a.rows, cols = a.cols)
}

fun matrix_mul_scalar(m: Matrix, k: Double): Matrix {
    var res: MutableList<MutableList<Double>> = mutableListOf<MutableList<Double>>()
    var i: Int = (0).toInt()
    while (i < m.rows) {
        var row: MutableList<Double> = mutableListOf<Double>()
        var j: Int = (0).toInt()
        while (j < m.cols) {
            row = run { val _tmp = row.toMutableList(); _tmp.add(((((m.data)[i]!!) as MutableList<Double>))[j]!! * k); _tmp }
            j = j + 1
        }
        res = run { val _tmp = res.toMutableList(); _tmp.add(row); _tmp }
        i = i + 1
    }
    return Matrix(data = res, rows = m.rows, cols = m.cols)
}

fun matrix_mul(a: Matrix, b: Matrix): Matrix {
    if (a.cols != b.rows) {
        return Matrix(data = mutableListOf<MutableList<Double>>(), rows = 0, cols = 0)
    }
    var res: MutableList<MutableList<Double>> = mutableListOf<MutableList<Double>>()
    var i: Int = (0).toInt()
    while (i < a.rows) {
        var row: MutableList<Double> = mutableListOf<Double>()
        var j: Int = (0).toInt()
        while (j < b.cols) {
            var sum: Double = 0.0
            var k: Int = (0).toInt()
            while (k < a.cols) {
                sum = sum + (((((a.data)[i]!!) as MutableList<Double>))[k]!! * ((((b.data)[k]!!) as MutableList<Double>))[j]!!)
                k = k + 1
            }
            row = run { val _tmp = row.toMutableList(); _tmp.add(sum); _tmp }
            j = j + 1
        }
        res = run { val _tmp = res.toMutableList(); _tmp.add(row); _tmp }
        i = i + 1
    }
    return Matrix(data = res, rows = a.rows, cols = b.cols)
}

fun matrix_transpose(m: Matrix): Matrix {
    var res: MutableList<MutableList<Double>> = mutableListOf<MutableList<Double>>()
    var c: Int = (0).toInt()
    while (c < m.cols) {
        var row: MutableList<Double> = mutableListOf<Double>()
        var r: Int = (0).toInt()
        while (r < m.rows) {
            row = run { val _tmp = row.toMutableList(); _tmp.add(((((m.data)[r]!!) as MutableList<Double>))[c]!!); _tmp }
            r = r + 1
        }
        res = run { val _tmp = res.toMutableList(); _tmp.add(row); _tmp }
        c = c + 1
    }
    return Matrix(data = res, rows = m.cols, cols = m.rows)
}

fun sherman_morrison(ainv: Matrix, u: Matrix, v: Matrix): Matrix {
    var vt: Matrix = matrix_transpose(v)
    var vu: Matrix = matrix_mul(matrix_mul(vt, ainv), u)
    var factor: Double = ((((vu.data)[0]!!) as MutableList<Double>))[0]!! + 1.0
    if (factor == 0.0) {
        return Matrix(data = mutableListOf<MutableList<Double>>(), rows = 0, cols = 0)
    }
    var term1: Matrix = matrix_mul(ainv, u)
    var term2: Matrix = matrix_mul(vt, ainv)
    var numerator: Matrix = matrix_mul(term1, term2)
    var scaled: Matrix = matrix_mul_scalar(numerator, 1.0 / factor)
    return matrix_sub(ainv, scaled)
}

fun user_main(): Unit {
    var ainv: Matrix = matrix_from_lists(mutableListOf(mutableListOf(1.0, 0.0, 0.0), mutableListOf(0.0, 1.0, 0.0), mutableListOf(0.0, 0.0, 1.0)))
    var u: Matrix = matrix_from_lists(mutableListOf(mutableListOf(1.0), mutableListOf(2.0), mutableListOf(0.0 - 3.0)))
    var v: Matrix = matrix_from_lists(mutableListOf(mutableListOf(4.0), mutableListOf(0.0 - 2.0), mutableListOf(5.0)))
    var result: Matrix = sherman_morrison(ainv, u, v)
    println(matrix_to_string(result))
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
