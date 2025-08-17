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

fun search_in_sorted_matrix(mat: MutableList<MutableList<Double>>, m: Int, n: Int, key: Double): Unit {
    var i: Int = (m - 1).toInt()
    var j: Int = (0).toInt()
    while ((i >= 0) && (j < n)) {
        if (key == ((mat[i]!!) as MutableList<Double>)[j]!!) {
            println((((("Key " + _numToStr(key)) + " found at row- ") + _numToStr(i + 1)) + " column- ") + _numToStr(j + 1))
            return
        }
        if (key < ((mat[i]!!) as MutableList<Double>)[j]!!) {
            i = i - 1
        } else {
            j = j + 1
        }
    }
    println(("Key " + _numToStr(key)) + " not found")
}

fun user_main(): Unit {
    var mat: MutableList<MutableList<Double>> = mutableListOf(mutableListOf(2.0, 5.0, 7.0), mutableListOf(4.0, 8.0, 13.0), mutableListOf(9.0, 11.0, 15.0), mutableListOf(12.0, 17.0, 20.0))
    search_in_sorted_matrix(mat, mat.size, (mat[0]!!).size, 5.0)
    search_in_sorted_matrix(mat, mat.size, (mat[0]!!).size, 21.0)
    var mat2: MutableList<MutableList<Double>> = mutableListOf(mutableListOf(2.1, 5.0, 7.0), mutableListOf(4.0, 8.0, 13.0), mutableListOf(9.0, 11.0, 15.0), mutableListOf(12.0, 17.0, 20.0))
    search_in_sorted_matrix(mat2, mat2.size, (mat2[0]!!).size, 2.1)
    search_in_sorted_matrix(mat2, mat2.size, (mat2[0]!!).size, 2.2)
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
