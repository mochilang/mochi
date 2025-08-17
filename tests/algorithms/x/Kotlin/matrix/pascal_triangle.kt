import java.math.BigInteger

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

fun populate_current_row(triangle: MutableList<MutableList<Int>>, current_row_idx: Int): MutableList<Int> {
    var row: MutableList<Int> = mutableListOf<Int>()
    var i: Int = (0).toInt()
    while (i <= current_row_idx) {
        if ((i == 0) || (i == current_row_idx)) {
            row = run { val _tmp = row.toMutableList(); _tmp.add(1); _tmp }
        } else {
            var left: Int = (((triangle[current_row_idx - 1]!!) as MutableList<Int>)[i - 1]!!).toInt()
            var right: Int = (((triangle[current_row_idx - 1]!!) as MutableList<Int>)[i]!!).toInt()
            row = run { val _tmp = row.toMutableList(); _tmp.add(left + right); _tmp }
        }
        i = i + 1
    }
    return row
}

fun generate_pascal_triangle(num_rows: Int): MutableList<MutableList<Int>> {
    if (num_rows <= 0) {
        return mutableListOf<MutableList<Int>>()
    }
    var triangle: MutableList<MutableList<Int>> = mutableListOf<MutableList<Int>>()
    var row_idx: Int = (0).toInt()
    while (row_idx < num_rows) {
        var row: MutableList<Int> = populate_current_row(triangle, row_idx)
        triangle = run { val _tmp = triangle.toMutableList(); _tmp.add(row); _tmp }
        row_idx = row_idx + 1
    }
    return triangle
}

fun row_to_string(row: MutableList<Int>, total_rows: Int, row_idx: Int): String {
    var line: String = ""
    var spaces: Int = ((total_rows - row_idx) - 1).toInt()
    var s: Int = (0).toInt()
    while (s < spaces) {
        line = line + " "
        s = s + 1
    }
    var c: Int = (0).toInt()
    while (c <= row_idx) {
        line = line + _numToStr(row[c]!!)
        if (c != row_idx) {
            line = line + " "
        }
        c = c + 1
    }
    return line
}

fun print_pascal_triangle(num_rows: Int): Unit {
    var triangle: MutableList<MutableList<Int>> = generate_pascal_triangle(num_rows)
    var r: Int = (0).toInt()
    while (r < num_rows) {
        var line: String = row_to_string(triangle[r]!!, num_rows, r)
        println(line)
        r = r + 1
    }
}

fun user_main(): Unit {
    print_pascal_triangle(5)
    println(generate_pascal_triangle(5).toString())
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
