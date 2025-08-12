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

fun add(matrices: MutableList<MutableList<MutableList<Double>>>): MutableList<MutableList<Double>> {
    var rows: Int = ((matrices[0]!!).size).toInt()
    var cols: Int = (((((matrices[0]!!) as MutableList<MutableList<Double>>))[0]!!).size).toInt()
    var r: Int = (0).toInt()
    var result: MutableList<MutableList<Double>> = mutableListOf<MutableList<Double>>()
    while (r < rows) {
        var row: MutableList<Double> = mutableListOf<Double>()
        var c: Int = (0).toInt()
        while (c < cols) {
            var sum: Double = 0.0
            var m: Int = (0).toInt()
            while (m < matrices.size) {
                sum = sum + ((((((matrices[m]!!) as MutableList<MutableList<Double>>))[r]!!) as MutableList<Double>))[c]!!
                m = m + 1
            }
            row = run { val _tmp = row.toMutableList(); _tmp.add(sum); _tmp }
            c = c + 1
        }
        result = run { val _tmp = result.toMutableList(); _tmp.add(row); _tmp }
        r = r + 1
    }
    return result
}

fun subtract(a: MutableList<MutableList<Double>>, b: MutableList<MutableList<Double>>): MutableList<MutableList<Double>> {
    var rows: Int = (a.size).toInt()
    var cols: Int = ((a[0]!!).size).toInt()
    var r: Int = (0).toInt()
    var result: MutableList<MutableList<Double>> = mutableListOf<MutableList<Double>>()
    while (r < rows) {
        var row: MutableList<Double> = mutableListOf<Double>()
        var c: Int = (0).toInt()
        while (c < cols) {
            row = run { val _tmp = row.toMutableList(); _tmp.add((((a[r]!!) as MutableList<Double>))[c]!! - (((b[r]!!) as MutableList<Double>))[c]!!); _tmp }
            c = c + 1
        }
        result = run { val _tmp = result.toMutableList(); _tmp.add(row); _tmp }
        r = r + 1
    }
    return result
}

fun scalar_multiply(matrix: MutableList<MutableList<Double>>, n: Double): MutableList<MutableList<Double>> {
    var result: MutableList<MutableList<Double>> = mutableListOf<MutableList<Double>>()
    var i: Int = (0).toInt()
    while (i < matrix.size) {
        var row: MutableList<Double> = mutableListOf<Double>()
        var j: Int = (0).toInt()
        while (j < (matrix[i]!!).size) {
            row = run { val _tmp = row.toMutableList(); _tmp.add((((matrix[i]!!) as MutableList<Double>))[j]!! * n); _tmp }
            j = j + 1
        }
        result = run { val _tmp = result.toMutableList(); _tmp.add(row); _tmp }
        i = i + 1
    }
    return result
}

fun multiply(a: MutableList<MutableList<Double>>, b: MutableList<MutableList<Double>>): MutableList<MutableList<Double>> {
    var rowsA: Int = (a.size).toInt()
    var colsA: Int = ((a[0]!!).size).toInt()
    var rowsB: Int = (b.size).toInt()
    var colsB: Int = ((b[0]!!).size).toInt()
    var result: MutableList<MutableList<Double>> = mutableListOf<MutableList<Double>>()
    var i: Int = (0).toInt()
    while (i < rowsA) {
        var row: MutableList<Double> = mutableListOf<Double>()
        var j: Int = (0).toInt()
        while (j < colsB) {
            var sum: Double = 0.0
            var k: Int = (0).toInt()
            while (k < colsA) {
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

fun identity(n: Int): MutableList<MutableList<Double>> {
    var result: MutableList<MutableList<Double>> = mutableListOf<MutableList<Double>>()
    var i: Int = (0).toInt()
    while (i < n) {
        var row: MutableList<Double> = mutableListOf<Double>()
        var j: Int = (0).toInt()
        while (j < n) {
            if (i == j) {
                row = run { val _tmp = row.toMutableList(); _tmp.add(1.0); _tmp }
            } else {
                row = run { val _tmp = row.toMutableList(); _tmp.add(0.0); _tmp }
            }
            j = j + 1
        }
        result = run { val _tmp = result.toMutableList(); _tmp.add(row); _tmp }
        i = i + 1
    }
    return result
}

fun transpose(matrix: MutableList<MutableList<Double>>): MutableList<MutableList<Double>> {
    var rows: Int = (matrix.size).toInt()
    var cols: Int = ((matrix[0]!!).size).toInt()
    var result: MutableList<MutableList<Double>> = mutableListOf<MutableList<Double>>()
    var c: Int = (0).toInt()
    while (c < cols) {
        var row: MutableList<Double> = mutableListOf<Double>()
        var r: Int = (0).toInt()
        while (r < rows) {
            row = run { val _tmp = row.toMutableList(); _tmp.add((((matrix[r]!!) as MutableList<Double>))[c]!!); _tmp }
            r = r + 1
        }
        result = run { val _tmp = result.toMutableList(); _tmp.add(row); _tmp }
        c = c + 1
    }
    return result
}

fun minor(matrix: MutableList<MutableList<Double>>, row: Int, column: Int): MutableList<MutableList<Double>> {
    var result: MutableList<MutableList<Double>> = mutableListOf<MutableList<Double>>()
    var i: Int = (0).toInt()
    while (i < matrix.size) {
        if (i != row) {
            var new_row: MutableList<Double> = mutableListOf<Double>()
            var j: Int = (0).toInt()
            while (j < (matrix[i]!!).size) {
                if (j != column) {
                    new_row = run { val _tmp = new_row.toMutableList(); _tmp.add((((matrix[i]!!) as MutableList<Double>))[j]!!); _tmp }
                }
                j = j + 1
            }
            result = run { val _tmp = result.toMutableList(); _tmp.add(new_row); _tmp }
        }
        i = i + 1
    }
    return result
}

fun determinant(matrix: MutableList<MutableList<Double>>): Double {
    if (matrix.size == 1) {
        return (((matrix[0]!!) as MutableList<Double>))[0]!!
    }
    var det: Double = 0.0
    var c: Int = (0).toInt()
    while (c < (matrix[0]!!).size) {
        var sub: MutableList<MutableList<Double>> = minor(matrix, 0, c)
        var sign: Double = (if ((Math.floorMod(c, 2)) == 0) 1.0 else 0.0 - 1.0.toDouble())
        det = det + (((((matrix[0]!!) as MutableList<Double>))[c]!! * determinant(sub)) * sign)
        c = c + 1
    }
    return det
}

fun inverse(matrix: MutableList<MutableList<Double>>): MutableList<MutableList<Double>> {
    var det: Double = determinant(matrix)
    if (det == 0.0) {
        return mutableListOf<MutableList<Double>>()
    }
    var size: Int = (matrix.size).toInt()
    var matrix_minor: MutableList<MutableList<Double>> = mutableListOf<MutableList<Double>>()
    var i: Int = (0).toInt()
    while (i < size) {
        var row: MutableList<Double> = mutableListOf<Double>()
        var j: Int = (0).toInt()
        while (j < size) {
            var m: MutableList<MutableList<Double>> = minor(matrix, i, j)
            row = run { val _tmp = row.toMutableList(); _tmp.add(determinant(m)); _tmp }
            j = j + 1
        }
        matrix_minor = run { val _tmp = matrix_minor.toMutableList(); _tmp.add(row); _tmp }
        i = i + 1
    }
    var cofactors: MutableList<MutableList<Double>> = mutableListOf<MutableList<Double>>()
    i = 0
    while (i < size) {
        var row: MutableList<Double> = mutableListOf<Double>()
        var j: Int = (0).toInt()
        while (j < size) {
            var sign: Double = (if ((Math.floorMod((i + j), 2)) == 0) 1.0 else 0.0 - 1.0.toDouble())
            row = run { val _tmp = row.toMutableList(); _tmp.add((((matrix_minor[i]!!) as MutableList<Double>))[j]!! * sign); _tmp }
            j = j + 1
        }
        cofactors = run { val _tmp = cofactors.toMutableList(); _tmp.add(row); _tmp }
        i = i + 1
    }
    var adjugate: MutableList<MutableList<Double>> = transpose(cofactors)
    return scalar_multiply(adjugate, 1.0 / det)
}

fun user_main(): Unit {
    var matrix_a: MutableList<MutableList<Double>> = mutableListOf(mutableListOf(12.0, 10.0), mutableListOf(3.0, 9.0))
    var matrix_b: MutableList<MutableList<Double>> = mutableListOf(mutableListOf(3.0, 4.0), mutableListOf(7.0, 4.0))
    var matrix_c: MutableList<MutableList<Double>> = mutableListOf(mutableListOf(11.0, 12.0, 13.0, 14.0), mutableListOf(21.0, 22.0, 23.0, 24.0), mutableListOf(31.0, 32.0, 33.0, 34.0), mutableListOf(41.0, 42.0, 43.0, 44.0))
    var matrix_d: MutableList<MutableList<Double>> = mutableListOf(mutableListOf(3.0, 0.0, 2.0), mutableListOf(2.0, 0.0, 0.0 - 2.0), mutableListOf(0.0, 1.0, 1.0))
    println(("Add Operation, add(matrix_a, matrix_b) = " + add(mutableListOf(matrix_a, matrix_b)).toString()) + " \n")
    println(("Multiply Operation, multiply(matrix_a, matrix_b) = " + multiply(matrix_a, matrix_b).toString()) + " \n")
    println(("Identity: " + identity(5).toString()) + "\n")
    println(((("Minor of " + matrix_c.toString()) + " = ") + minor(matrix_c, 1, 2).toString()) + " \n")
    println(((("Determinant of " + matrix_b.toString()) + " = ") + determinant(matrix_b).toString()) + " \n")
    println(((("Inverse of " + matrix_d.toString()) + " = ") + inverse(matrix_d).toString()) + "\n")
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
