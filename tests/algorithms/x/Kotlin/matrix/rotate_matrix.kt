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

var mat: MutableList<MutableList<Int>> = make_matrix(4)
fun abs_int(n: Int): Int {
    if (n < 0) {
        return 0 - n
    }
    return n
}

fun make_matrix(row_size: Int): MutableList<MutableList<Int>> {
    var size: Int = (abs_int(row_size)).toInt()
    if (size == 0) {
        size = 4
    }
    var mat: MutableList<MutableList<Int>> = mutableListOf<MutableList<Int>>()
    var y: Int = (0).toInt()
    while (y < size) {
        var row: MutableList<Int> = mutableListOf<Int>()
        var x: Int = (0).toInt()
        while (x < size) {
            row = run { val _tmp = row.toMutableList(); _tmp.add((1 + x) + (y * size)); _tmp }
            x = x + 1
        }
        mat = run { val _tmp = mat.toMutableList(); _tmp.add(row); _tmp }
        y = y + 1
    }
    return mat
}

fun transpose(mat: MutableList<MutableList<Int>>): MutableList<MutableList<Int>> {
    var n: Int = (mat.size).toInt()
    var result: MutableList<MutableList<Int>> = mutableListOf<MutableList<Int>>()
    var i: Int = (0).toInt()
    while (i < n) {
        var row: MutableList<Int> = mutableListOf<Int>()
        var j: Int = (0).toInt()
        while (j < n) {
            row = run { val _tmp = row.toMutableList(); _tmp.add((((mat[j]!!) as MutableList<Int>))[i]!!); _tmp }
            j = j + 1
        }
        result = run { val _tmp = result.toMutableList(); _tmp.add(row); _tmp }
        i = i + 1
    }
    return result
}

fun reverse_row(mat: MutableList<MutableList<Int>>): MutableList<MutableList<Int>> {
    var result: MutableList<MutableList<Int>> = mutableListOf<MutableList<Int>>()
    var i: BigInteger = ((mat.size - 1).toBigInteger())
    while (i.compareTo((0).toBigInteger()) >= 0) {
        result = run { val _tmp = result.toMutableList(); _tmp.add(mat[(i).toInt()]!!); _tmp }
        i = i.subtract((1).toBigInteger())
    }
    return result
}

fun reverse_column(mat: MutableList<MutableList<Int>>): MutableList<MutableList<Int>> {
    var result: MutableList<MutableList<Int>> = mutableListOf<MutableList<Int>>()
    var i: Int = (0).toInt()
    while (i < mat.size) {
        var row: MutableList<Int> = mutableListOf<Int>()
        var j: BigInteger = (((mat[i]!!).size - 1).toBigInteger())
        while (j.compareTo((0).toBigInteger()) >= 0) {
            row = run { val _tmp = row.toMutableList(); _tmp.add((((mat[i]!!) as MutableList<Int>))[(j).toInt()]!!); _tmp }
            j = j.subtract((1).toBigInteger())
        }
        result = run { val _tmp = result.toMutableList(); _tmp.add(row); _tmp }
        i = i + 1
    }
    return result
}

fun rotate_90(mat: MutableList<MutableList<Int>>): MutableList<MutableList<Int>> {
    var t: MutableList<MutableList<Int>> = transpose(mat)
    var rr: MutableList<MutableList<Int>> = reverse_row(t)
    return rr
}

fun rotate_180(mat: MutableList<MutableList<Int>>): MutableList<MutableList<Int>> {
    var rc: MutableList<MutableList<Int>> = reverse_column(mat)
    var rr: MutableList<MutableList<Int>> = reverse_row(rc)
    return rr
}

fun rotate_270(mat: MutableList<MutableList<Int>>): MutableList<MutableList<Int>> {
    var t: MutableList<MutableList<Int>> = transpose(mat)
    var rc: MutableList<MutableList<Int>> = reverse_column(t)
    return rc
}

fun row_to_string(row: MutableList<Int>): String {
    var line: String = ""
    var i: Int = (0).toInt()
    while (i < row.size) {
        if (i == 0) {
            line = (row[i]!!).toString()
        } else {
            line = (line + " ") + (row[i]!!).toString()
        }
        i = i + 1
    }
    return line
}

fun print_matrix(mat: MutableList<MutableList<Int>>): Unit {
    var i: Int = (0).toInt()
    while (i < mat.size) {
        println(row_to_string(mat[i]!!))
        i = i + 1
    }
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println("\norigin:\n")
        print_matrix(mat)
        println("\nrotate 90 counterclockwise:\n")
        var r90: MutableList<MutableList<Int>> = rotate_90(mat)
        print_matrix(r90)
        mat = make_matrix(4)
        println("\norigin:\n")
        print_matrix(mat)
        println("\nrotate 180:\n")
        var r180: MutableList<MutableList<Int>> = rotate_180(mat)
        print_matrix(r180)
        mat = make_matrix(4)
        println("\norigin:\n")
        print_matrix(mat)
        println("\nrotate 270 counterclockwise:\n")
        var r270: MutableList<MutableList<Int>> = rotate_270(mat)
        print_matrix(r270)
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
