fun _numToStr(v: Number): String {
    val d = v.toDouble()
    val i = d.toLong()
    return if (d == i.toDouble()) i.toString() else d.toString()
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

fun toJson(v: Any?): String = when (v) {
    null -> "null"
    is String -> "\"" + v.replace("\"", "\\\"") + "\""
    is Boolean, is Number -> v.toString()
    is Map<*, *> -> v.entries.joinToString(prefix = "{", postfix = "}") { toJson(it.key.toString()) + ":" + toJson(it.value) }
    is Iterable<*> -> v.joinToString(prefix = "[", postfix = "]") { toJson(it) }
    else -> toJson(v.toString())
}

data class Matrix(var data: MutableList<MutableList<Double>> = mutableListOf<MutableList<Double>>(), var rows: Int = 0, var cols: Int = 0)
fun make_matrix(values: MutableList<MutableList<Double>>): Matrix {
    var r: Int = (values.size).toInt()
    if (r == 0) {
        return Matrix(data = mutableListOf<MutableList<Double>>(), rows = 0, cols = 0)
    }
    var c: Int = ((values[0]!!).size).toInt()
    var i: Int = (0).toInt()
    while (i < r) {
        if ((values[i]!!).size != c) {
            return Matrix(data = mutableListOf<MutableList<Double>>(), rows = 0, cols = 0)
        }
        i = i + 1
    }
    return Matrix(data = values, rows = r, cols = c)
}

fun matrix_columns(m: Matrix): MutableList<MutableList<Double>> {
    var cols: MutableList<MutableList<Double>> = mutableListOf<MutableList<Double>>()
    var j: Int = (0).toInt()
    while (j < m.cols) {
        var col: MutableList<Double> = mutableListOf<Double>()
        var i: Int = (0).toInt()
        while (i < m.rows) {
            col = run { val _tmp = col.toMutableList(); _tmp.add((((m.data)[i]!!) as MutableList<Double>)[j]!!); _tmp }
            i = i + 1
        }
        cols = run { val _tmp = cols.toMutableList(); _tmp.add(col); _tmp }
        j = j + 1
    }
    return cols
}

fun matrix_identity(m: Matrix): Matrix {
    var vals: MutableList<MutableList<Double>> = mutableListOf<MutableList<Double>>()
    var i: Int = (0).toInt()
    while (i < m.rows) {
        var row: MutableList<Double> = mutableListOf<Double>()
        var j: Int = (0).toInt()
        while (j < m.cols) {
            var v: Double = if (i == j) 1.0 else 0.0.toDouble()
            row = run { val _tmp = row.toMutableList(); _tmp.add(v); _tmp }
            j = j + 1
        }
        vals = run { val _tmp = vals.toMutableList(); _tmp.add(row); _tmp }
        i = i + 1
    }
    return Matrix(data = vals, rows = m.rows, cols = m.cols)
}

fun matrix_minor(m: Matrix, r: Int, c: Int): Double {
    var vals: MutableList<MutableList<Double>> = mutableListOf<MutableList<Double>>()
    var i: Int = (0).toInt()
    while (i < m.rows) {
        if (i != r) {
            var row: MutableList<Double> = mutableListOf<Double>()
            var j: Int = (0).toInt()
            while (j < m.cols) {
                if (j != c) {
                    row = run { val _tmp = row.toMutableList(); _tmp.add((((m.data)[i]!!) as MutableList<Double>)[j]!!); _tmp }
                }
                j = j + 1
            }
            vals = run { val _tmp = vals.toMutableList(); _tmp.add(row); _tmp }
        }
        i = i + 1
    }
    var sub: Matrix = Matrix(data = vals, rows = m.rows - 1, cols = m.cols - 1)
    return matrix_determinant(sub)
}

fun matrix_cofactor(m: Matrix, r: Int, c: Int): Double {
    var minor: Double = matrix_minor(m, r, c)
    if ((Math.floorMod((r + c), 2)) == 0) {
        return minor
    }
    return (0.0 - 1.0) * minor
}

fun matrix_minors(m: Matrix): Matrix {
    var vals: MutableList<MutableList<Double>> = mutableListOf<MutableList<Double>>()
    var i: Int = (0).toInt()
    while (i < m.rows) {
        var row: MutableList<Double> = mutableListOf<Double>()
        var j: Int = (0).toInt()
        while (j < m.cols) {
            row = run { val _tmp = row.toMutableList(); _tmp.add(matrix_minor(m, i, j)); _tmp }
            j = j + 1
        }
        vals = run { val _tmp = vals.toMutableList(); _tmp.add(row); _tmp }
        i = i + 1
    }
    return Matrix(data = vals, rows = m.rows, cols = m.cols)
}

fun matrix_cofactors(m: Matrix): Matrix {
    var vals: MutableList<MutableList<Double>> = mutableListOf<MutableList<Double>>()
    var i: Int = (0).toInt()
    while (i < m.rows) {
        var row: MutableList<Double> = mutableListOf<Double>()
        var j: Int = (0).toInt()
        while (j < m.cols) {
            row = run { val _tmp = row.toMutableList(); _tmp.add(matrix_cofactor(m, i, j)); _tmp }
            j = j + 1
        }
        vals = run { val _tmp = vals.toMutableList(); _tmp.add(row); _tmp }
        i = i + 1
    }
    return Matrix(data = vals, rows = m.rows, cols = m.cols)
}

fun matrix_determinant(m: Matrix): Double {
    if (m.rows != m.cols) {
        return 0.0
    }
    if (m.rows == 0) {
        return 0.0
    }
    if (m.rows == 1) {
        return (((m.data)[0]!!) as MutableList<Double>)[0]!!
    }
    if (m.rows == 2) {
        return ((((m.data)[0]!!) as MutableList<Double>)[0]!! * (((m.data)[1]!!) as MutableList<Double>)[1]!!) - ((((m.data)[0]!!) as MutableList<Double>)[1]!! * (((m.data)[1]!!) as MutableList<Double>)[0]!!)
    }
    var sum: Double = 0.0
    var j: Int = (0).toInt()
    while (j < m.cols) {
        sum = sum + ((((m.data)[0]!!) as MutableList<Double>)[j]!! * matrix_cofactor(m, 0, j))
        j = j + 1
    }
    return sum
}

fun matrix_is_invertible(m: Matrix): Boolean {
    return matrix_determinant(m) != 0.0
}

fun matrix_adjugate(m: Matrix): Matrix {
    var cof: Matrix = matrix_cofactors(m)
    var vals: MutableList<MutableList<Double>> = mutableListOf<MutableList<Double>>()
    var i: Int = (0).toInt()
    while (i < m.rows) {
        var row: MutableList<Double> = mutableListOf<Double>()
        var j: Int = (0).toInt()
        while (j < m.cols) {
            row = run { val _tmp = row.toMutableList(); _tmp.add((((cof.data)[j]!!) as MutableList<Double>)[i]!!); _tmp }
            j = j + 1
        }
        vals = run { val _tmp = vals.toMutableList(); _tmp.add(row); _tmp }
        i = i + 1
    }
    return Matrix(data = vals, rows = m.rows, cols = m.cols)
}

fun matrix_inverse(m: Matrix): Matrix {
    var det: Double = matrix_determinant(m)
    if (det == 0.0) {
        return Matrix(data = mutableListOf<MutableList<Double>>(), rows = 0, cols = 0)
    }
    var adj: Matrix = matrix_adjugate(m)
    return (matrix_mul_scalar(adj, 1.0 / det)) as Matrix
}

fun matrix_add_row(m: Matrix, row: MutableList<Double>): Matrix {
    var newData: MutableList<MutableList<Double>> = m.data
    newData = run { val _tmp = newData.toMutableList(); _tmp.add(row); _tmp }
    return Matrix(data = newData, rows = m.rows + 1, cols = m.cols)
}

fun matrix_add_column(m: Matrix, col: MutableList<Double>): Matrix {
    var newData: MutableList<MutableList<Double>> = mutableListOf<MutableList<Double>>()
    var i: Int = (0).toInt()
    while (i < m.rows) {
        newData = run { val _tmp = newData.toMutableList(); _tmp.add(run { val _tmp = ((m.data)[i]!!).toMutableList(); _tmp.add(col[i]!!); _tmp }); _tmp }
        i = i + 1
    }
    return Matrix(data = newData, rows = m.rows, cols = m.cols + 1)
}

fun matrix_mul_scalar(m: Matrix, s: Double): Matrix {
    var vals: MutableList<MutableList<Double>> = mutableListOf<MutableList<Double>>()
    var i: Int = (0).toInt()
    while (i < m.rows) {
        var row: MutableList<Double> = mutableListOf<Double>()
        var j: Int = (0).toInt()
        while (j < m.cols) {
            row = run { val _tmp = row.toMutableList(); _tmp.add((((m.data)[i]!!) as MutableList<Double>)[j]!! * s); _tmp }
            j = j + 1
        }
        vals = run { val _tmp = vals.toMutableList(); _tmp.add(row); _tmp }
        i = i + 1
    }
    return Matrix(data = vals, rows = m.rows, cols = m.cols)
}

fun matrix_neg(m: Matrix): Matrix {
    return matrix_mul_scalar(m, 0.0 - 1.0)
}

fun matrix_add(a: Matrix, b: Matrix): Matrix {
    if ((a.rows != b.rows) || (a.cols != b.cols)) {
        return Matrix(data = mutableListOf<MutableList<Double>>(), rows = 0, cols = 0)
    }
    var vals: MutableList<MutableList<Double>> = mutableListOf<MutableList<Double>>()
    var i: Int = (0).toInt()
    while (i < a.rows) {
        var row: MutableList<Double> = mutableListOf<Double>()
        var j: Int = (0).toInt()
        while (j < a.cols) {
            row = run { val _tmp = row.toMutableList(); _tmp.add((((a.data)[i]!!) as MutableList<Double>)[j]!! + (((b.data)[i]!!) as MutableList<Double>)[j]!!); _tmp }
            j = j + 1
        }
        vals = run { val _tmp = vals.toMutableList(); _tmp.add(row); _tmp }
        i = i + 1
    }
    return Matrix(data = vals, rows = a.rows, cols = a.cols)
}

fun matrix_sub(a: Matrix, b: Matrix): Matrix {
    if ((a.rows != b.rows) || (a.cols != b.cols)) {
        return Matrix(data = mutableListOf<MutableList<Double>>(), rows = 0, cols = 0)
    }
    var vals: MutableList<MutableList<Double>> = mutableListOf<MutableList<Double>>()
    var i: Int = (0).toInt()
    while (i < a.rows) {
        var row: MutableList<Double> = mutableListOf<Double>()
        var j: Int = (0).toInt()
        while (j < a.cols) {
            row = run { val _tmp = row.toMutableList(); _tmp.add((((a.data)[i]!!) as MutableList<Double>)[j]!! - (((b.data)[i]!!) as MutableList<Double>)[j]!!); _tmp }
            j = j + 1
        }
        vals = run { val _tmp = vals.toMutableList(); _tmp.add(row); _tmp }
        i = i + 1
    }
    return Matrix(data = vals, rows = a.rows, cols = a.cols)
}

fun matrix_dot(row: MutableList<Double>, col: MutableList<Double>): Double {
    var sum: Double = 0.0
    var i: Int = (0).toInt()
    while (i < row.size) {
        sum = sum + (row[i]!! * col[i]!!)
        i = i + 1
    }
    return sum
}

fun matrix_mul(a: Matrix, b: Matrix): Matrix {
    if (a.cols != b.rows) {
        return Matrix(data = mutableListOf<MutableList<Double>>(), rows = 0, cols = 0)
    }
    var bcols: MutableList<MutableList<Double>> = matrix_columns(b)
    var vals: MutableList<MutableList<Double>> = mutableListOf<MutableList<Double>>()
    var i: Int = (0).toInt()
    while (i < a.rows) {
        var row: MutableList<Double> = mutableListOf<Double>()
        var j: Int = (0).toInt()
        while (j < b.cols) {
            row = run { val _tmp = row.toMutableList(); _tmp.add(matrix_dot((a.data)[i]!!, bcols[j]!!)); _tmp }
            j = j + 1
        }
        vals = run { val _tmp = vals.toMutableList(); _tmp.add(row); _tmp }
        i = i + 1
    }
    return Matrix(data = vals, rows = a.rows, cols = b.cols)
}

fun matrix_pow(m: Matrix, p: Int): Matrix {
    if (p == 0) {
        return matrix_identity(m)
    }
    if (p < 0) {
        if ((matrix_is_invertible(m)) as Boolean) {
            return matrix_pow(matrix_inverse(m), 0 - p)
        }
        return Matrix(data = mutableListOf<MutableList<Double>>(), rows = 0, cols = 0)
    }
    var result: Matrix = m
    var i: Int = (1).toInt()
    while (i < p) {
        result = matrix_mul(result, m)
        i = i + 1
    }
    return result
}

fun matrix_to_string(m: Matrix): String {
    if (m.rows == 0) {
        return "[]"
    }
    var s: String = "["
    var i: Int = (0).toInt()
    while (i < m.rows) {
        s = s + "["
        var j: Int = (0).toInt()
        while (j < m.cols) {
            s = s + _numToStr((((m.data)[i]!!) as MutableList<Double>)[j]!!)
            if (j < (m.cols - 1)) {
                s = s + " "
            }
            j = j + 1
        }
        s = s + "]"
        if (i < (m.rows - 1)) {
            s = s + "\n "
        }
        i = i + 1
    }
    s = s + "]"
    return s
}

fun user_main(): Unit {
    var m: Matrix = make_matrix(mutableListOf(mutableListOf(1.0, 2.0, 3.0), mutableListOf(4.0, 5.0, 6.0), mutableListOf(7.0, 8.0, 9.0)))
    println(matrix_to_string(m))
    println(matrix_columns(m).toString())
    println((_numToStr(m.rows) + ",") + _numToStr(m.cols))
    println(matrix_is_invertible(m).toString())
    println(matrix_to_string(matrix_identity(m)))
    println(_numToStr(matrix_determinant(m)))
    println(matrix_to_string(matrix_minors(m)))
    println(matrix_to_string(matrix_cofactors(m)))
    println(matrix_to_string(matrix_adjugate(m)))
    var m2: Matrix = matrix_mul_scalar(m, 3.0)
    println(matrix_to_string(m2))
    println(matrix_to_string(matrix_add(m, m2)))
    println(matrix_to_string(matrix_sub(m, m2)))
    println(matrix_to_string(matrix_pow(m, 3)))
    var m3: Matrix = matrix_add_row(m, mutableListOf(10.0, 11.0, 12.0))
    println(matrix_to_string(m3))
    var m4: Matrix = matrix_add_column(m2, mutableListOf(8.0, 16.0, 32.0))
    println(matrix_to_string(matrix_mul(m3, m4)))
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
