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

fun check_matrix(mat: MutableList<MutableList<Double>>): Unit {
    if ((mat.size < 2) || ((mat[0]!!).size < 2)) {
        panic("Expected a matrix with at least 2x2 dimensions")
    }
}

fun add(a: MutableList<MutableList<Double>>, b: MutableList<MutableList<Double>>): MutableList<MutableList<Double>> {
    check_matrix(a)
    check_matrix(b)
    if ((a.size != b.size) || ((a[0]!!).size != (b[0]!!).size)) {
        panic("Matrices must have the same dimensions")
    }
    var rows: Int = (a.size).toInt()
    var cols: Int = ((a[0]!!).size).toInt()
    var result: MutableList<MutableList<Double>> = mutableListOf<MutableList<Double>>()
    var i: Int = (0).toInt()
    while (i < rows) {
        var row: MutableList<Double> = mutableListOf<Double>()
        var j: Int = (0).toInt()
        while (j < cols) {
            row = run { val _tmp = row.toMutableList(); _tmp.add(((a[i]!!) as MutableList<Double>)[j]!! + ((b[i]!!) as MutableList<Double>)[j]!!); _tmp }
            j = j + 1
        }
        result = run { val _tmp = result.toMutableList(); _tmp.add(row); _tmp }
        i = i + 1
    }
    return result
}

fun subtract(a: MutableList<MutableList<Double>>, b: MutableList<MutableList<Double>>): MutableList<MutableList<Double>> {
    check_matrix(a)
    check_matrix(b)
    if ((a.size != b.size) || ((a[0]!!).size != (b[0]!!).size)) {
        panic("Matrices must have the same dimensions")
    }
    var rows: Int = (a.size).toInt()
    var cols: Int = ((a[0]!!).size).toInt()
    var result: MutableList<MutableList<Double>> = mutableListOf<MutableList<Double>>()
    var i: Int = (0).toInt()
    while (i < rows) {
        var row: MutableList<Double> = mutableListOf<Double>()
        var j: Int = (0).toInt()
        while (j < cols) {
            row = run { val _tmp = row.toMutableList(); _tmp.add(((a[i]!!) as MutableList<Double>)[j]!! - ((b[i]!!) as MutableList<Double>)[j]!!); _tmp }
            j = j + 1
        }
        result = run { val _tmp = result.toMutableList(); _tmp.add(row); _tmp }
        i = i + 1
    }
    return result
}

fun scalar_multiply(a: MutableList<MutableList<Double>>, s: Double): MutableList<MutableList<Double>> {
    check_matrix(a)
    var rows: Int = (a.size).toInt()
    var cols: Int = ((a[0]!!).size).toInt()
    var result: MutableList<MutableList<Double>> = mutableListOf<MutableList<Double>>()
    var i: Int = (0).toInt()
    while (i < rows) {
        var row: MutableList<Double> = mutableListOf<Double>()
        var j: Int = (0).toInt()
        while (j < cols) {
            row = run { val _tmp = row.toMutableList(); _tmp.add(((a[i]!!) as MutableList<Double>)[j]!! * s); _tmp }
            j = j + 1
        }
        result = run { val _tmp = result.toMutableList(); _tmp.add(row); _tmp }
        i = i + 1
    }
    return result
}

fun multiply(a: MutableList<MutableList<Double>>, b: MutableList<MutableList<Double>>): MutableList<MutableList<Double>> {
    check_matrix(a)
    check_matrix(b)
    if ((a[0]!!).size != b.size) {
        panic("Invalid dimensions for matrix multiplication")
    }
    var rows: Int = (a.size).toInt()
    var cols: Int = ((b[0]!!).size).toInt()
    var result: MutableList<MutableList<Double>> = mutableListOf<MutableList<Double>>()
    var i: Int = (0).toInt()
    while (i < rows) {
        var row: MutableList<Double> = mutableListOf<Double>()
        var j: Int = (0).toInt()
        while (j < cols) {
            var sum: Double = 0.0
            var k: Int = (0).toInt()
            while (k < b.size) {
                sum = sum + (((a[i]!!) as MutableList<Double>)[k]!! * ((b[k]!!) as MutableList<Double>)[j]!!)
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

fun transpose(a: MutableList<MutableList<Double>>): MutableList<MutableList<Double>> {
    check_matrix(a)
    var rows: Int = (a.size).toInt()
    var cols: Int = ((a[0]!!).size).toInt()
    var result: MutableList<MutableList<Double>> = mutableListOf<MutableList<Double>>()
    var j: Int = (0).toInt()
    while (j < cols) {
        var row: MutableList<Double> = mutableListOf<Double>()
        var i: Int = (0).toInt()
        while (i < rows) {
            row = run { val _tmp = row.toMutableList(); _tmp.add(((a[i]!!) as MutableList<Double>)[j]!!); _tmp }
            i = i + 1
        }
        result = run { val _tmp = result.toMutableList(); _tmp.add(row); _tmp }
        j = j + 1
    }
    return result
}

fun user_main(): Unit {
    var mat_a: MutableList<MutableList<Double>> = mutableListOf(mutableListOf(12.0, 10.0), mutableListOf(3.0, 9.0))
    var mat_b: MutableList<MutableList<Double>> = mutableListOf(mutableListOf(3.0, 4.0), mutableListOf(7.0, 4.0))
    var mat_c: MutableList<MutableList<Double>> = mutableListOf(mutableListOf(3.0, 0.0, 2.0), mutableListOf(2.0, 0.0, 0.0 - 2.0), mutableListOf(0.0, 1.0, 1.0))
    println(add(mat_a, mat_b).toString())
    println(subtract(mat_a, mat_b).toString())
    println(multiply(mat_a, mat_b).toString())
    println(scalar_multiply(mat_a, 3.5).toString())
    println(identity(5).toString())
    println(transpose(mat_c).toString())
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
