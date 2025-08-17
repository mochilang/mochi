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

fun identity(n: Int): MutableList<MutableList<Int>> {
    var i: Int = (0).toInt()
    var mat: MutableList<MutableList<Int>> = mutableListOf<MutableList<Int>>()
    while (i < n) {
        var row: MutableList<Int> = mutableListOf<Int>()
        var j: Int = (0).toInt()
        while (j < n) {
            if (i == j) {
                row = run { val _tmp = row.toMutableList(); _tmp.add(1); _tmp }
            } else {
                row = run { val _tmp = row.toMutableList(); _tmp.add(0); _tmp }
            }
            j = j + 1
        }
        mat = run { val _tmp = mat.toMutableList(); _tmp.add(row); _tmp }
        i = i + 1
    }
    return mat
}

fun matrix_mul(a: MutableList<MutableList<Int>>, b: MutableList<MutableList<Int>>): MutableList<MutableList<Int>> {
    var n: Int = (a.size).toInt()
    var result: MutableList<MutableList<Int>> = mutableListOf<MutableList<Int>>()
    var i: Int = (0).toInt()
    while (i < n) {
        var row: MutableList<Int> = mutableListOf<Int>()
        var j: Int = (0).toInt()
        while (j < n) {
            var cell: Int = (0).toInt()
            var k: Int = (0).toInt()
            while (k < n) {
                cell = cell + (((a[i]!!) as MutableList<Int>)[k]!! * ((b[k]!!) as MutableList<Int>)[j]!!)
                k = k + 1
            }
            row = run { val _tmp = row.toMutableList(); _tmp.add(cell); _tmp }
            j = j + 1
        }
        result = run { val _tmp = result.toMutableList(); _tmp.add(row); _tmp }
        i = i + 1
    }
    return result
}

fun matrix_pow(base: MutableList<MutableList<Int>>, exp: Int): MutableList<MutableList<Int>> {
    var result: MutableList<MutableList<Int>> = identity(base.size)
    var b: MutableList<MutableList<Int>> = base
    var e: Int = (exp).toInt()
    while (e > 0) {
        if ((Math.floorMod(e, 2)) == 1) {
            result = matrix_mul(result, b)
        }
        b = matrix_mul(b, b)
        e = e / 2
    }
    return result
}

fun fibonacci_with_matrix_exponentiation(n: Int, f1: Int, f2: Int): Int {
    if (n == 1) {
        return f1
    }
    if (n == 2) {
        return f2
    }
    var base: MutableList<MutableList<Int>> = mutableListOf(mutableListOf(1, 1), mutableListOf(1, 0))
    var m: MutableList<MutableList<Int>> = matrix_pow(base, n - 2)
    return (f2 * ((m[0]!!) as MutableList<Int>)[0]!!) + (f1 * ((m[0]!!) as MutableList<Int>)[1]!!)
}

fun simple_fibonacci(n: Int, f1: Int, f2: Int): Int {
    if (n == 1) {
        return f1
    }
    if (n == 2) {
        return f2
    }
    var a: Int = (f1).toInt()
    var b: Int = (f2).toInt()
    var count: Int = (n - 2).toInt()
    while (count > 0) {
        var tmp: Int = (a + b).toInt()
        a = b
        b = tmp
        count = count - 1
    }
    return b
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println(_numToStr(fibonacci_with_matrix_exponentiation(1, 5, 6)))
        println(_numToStr(fibonacci_with_matrix_exponentiation(2, 10, 11)))
        println(_numToStr(fibonacci_with_matrix_exponentiation(13, 0, 1)))
        println(_numToStr(fibonacci_with_matrix_exponentiation(10, 5, 9)))
        println(_numToStr(fibonacci_with_matrix_exponentiation(9, 2, 3)))
        println(_numToStr(simple_fibonacci(1, 5, 6)))
        println(_numToStr(simple_fibonacci(2, 10, 11)))
        println(_numToStr(simple_fibonacci(13, 0, 1)))
        println(_numToStr(simple_fibonacci(10, 5, 9)))
        println(_numToStr(simple_fibonacci(9, 2, 3)))
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
