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

fun multiply(matrix_a: MutableList<MutableList<Int>>, matrix_b: MutableList<MutableList<Int>>): MutableList<MutableList<Int>> {
    var n: Int = (matrix_a.size).toInt()
    var matrix_c: MutableList<MutableList<Int>> = mutableListOf<MutableList<Int>>()
    var i: Int = (0).toInt()
    while (i < n) {
        var row: MutableList<Int> = mutableListOf<Int>()
        var j: Int = (0).toInt()
        while (j < n) {
            var _val: Int = (0).toInt()
            var k: Int = (0).toInt()
            while (k < n) {
                _val = _val + (((matrix_a[i]!!) as MutableList<Int>)[k]!! * ((matrix_b[k]!!) as MutableList<Int>)[j]!!)
                k = k + 1
            }
            row = run { val _tmp = row.toMutableList(); _tmp.add(_val); _tmp }
            j = j + 1
        }
        matrix_c = run { val _tmp = matrix_c.toMutableList(); _tmp.add(row); _tmp }
        i = i + 1
    }
    return matrix_c
}

fun identity(n: Int): MutableList<MutableList<Int>> {
    var res: MutableList<MutableList<Int>> = mutableListOf<MutableList<Int>>()
    var i: Int = (0).toInt()
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
        res = run { val _tmp = res.toMutableList(); _tmp.add(row); _tmp }
        i = i + 1
    }
    return res
}

fun nth_fibonacci_matrix(n: Int): Int {
    if (n <= 1) {
        return n
    }
    var res_matrix: MutableList<MutableList<Int>> = identity(2)
    var fib_matrix: MutableList<MutableList<Int>> = mutableListOf(mutableListOf(1, 1), mutableListOf(1, 0))
    var m: Int = (n - 1).toInt()
    while (m > 0) {
        if ((Math.floorMod(m, 2)) == 1) {
            res_matrix = multiply(res_matrix, fib_matrix)
        }
        fib_matrix = multiply(fib_matrix, fib_matrix)
        m = m / 2
    }
    return ((res_matrix[0]!!) as MutableList<Int>)[0]!!
}

fun nth_fibonacci_bruteforce(n: Int): Int {
    if (n <= 1) {
        return n
    }
    var fib0: Int = (0).toInt()
    var fib1: Int = (1).toInt()
    var i: Int = (2).toInt()
    while (i <= n) {
        var next: Int = (fib0 + fib1).toInt()
        fib0 = fib1
        fib1 = next
        i = i + 1
    }
    return fib1
}

fun parse_number(s: String): Int {
    var result: Int = (0).toInt()
    var i: Int = (0).toInt()
    while (i < s.length) {
        var ch: String = s.substring(i, i + 1)
        if ((ch >= "0") && (ch <= "9")) {
            result = (result * 10) + (ch.toBigInteger().toInt())
        }
        i = i + 1
    }
    return result
}

fun user_main(): Unit {
    var ordinals: MutableList<String> = mutableListOf("0th", "1st", "2nd", "3rd", "10th", "100th", "1000th")
    var i: Int = (0).toInt()
    while (i < ordinals.size) {
        var ordinal: String = ordinals[i]!!
        var n: Int = (parse_number(ordinal)).toInt()
        var msg: String = (((ordinal + " fibonacci number using matrix exponentiation is ") + _numToStr(nth_fibonacci_matrix(n))) + " and using bruteforce is ") + _numToStr(nth_fibonacci_bruteforce(n))
        println(msg)
        i = i + 1
    }
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
