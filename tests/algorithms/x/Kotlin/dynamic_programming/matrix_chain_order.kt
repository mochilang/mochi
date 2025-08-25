import java.math.BigInteger

val _dataDir = "/workspace/mochi/tests/github/TheAlgorithms/Mochi/dynamic_programming"

fun <T> _listSet(lst: MutableList<T>, idx: Int, v: T) { while (lst.size <= idx) lst.add(v); lst[idx] = v }

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

data class MatrixChainResult(var matrix: MutableList<MutableList<Int>> = mutableListOf<MutableList<Int>>(), var solution: MutableList<MutableList<Int>> = mutableListOf<MutableList<Int>>())
fun make_2d(n: Int): MutableList<MutableList<Int>> {
    var res: MutableList<MutableList<Int>> = mutableListOf<MutableList<Int>>()
    var i: Int = (0).toInt()
    while (i < n) {
        var row: MutableList<Int> = mutableListOf<Int>()
        var j: Int = (0).toInt()
        while (j < n) {
            row = run { val _tmp = row.toMutableList(); _tmp.add(0); _tmp }
            j = j + 1
        }
        res = run { val _tmp = res.toMutableList(); _tmp.add(row); _tmp }
        i = i + 1
    }
    return res
}

fun matrix_chain_order(arr: MutableList<Int>): MatrixChainResult {
    var n: Int = (arr.size).toInt()
    var m: MutableList<MutableList<Int>> = make_2d(n)
    var s: MutableList<MutableList<Int>> = make_2d(n)
    var chain_length: Int = (2).toInt()
    while (chain_length < n) {
        var a: Int = (1).toInt()
        while (a < ((n - chain_length) + 1)) {
            var b: Int = ((a + chain_length) - 1).toInt()
            _listSet(m[a]!!, b, 1000000000)
            var c: Int = (a).toInt()
            while (c < b) {
                var cost: Int = ((((m[a]!!) as MutableList<Int>)[c]!! + ((m[c + 1]!!) as MutableList<Int>)[b]!!) + ((arr[a - 1]!! * arr[c]!!) * arr[b]!!)).toInt()
                if (cost < ((m[a]!!) as MutableList<Int>)[b]!!) {
                    _listSet(m[a]!!, b, cost)
                    _listSet(s[a]!!, b, c)
                }
                c = c + 1
            }
            a = a + 1
        }
        chain_length = chain_length + 1
    }
    return MatrixChainResult(matrix = m, solution = s)
}

fun optimal_parenthesization(s: MutableList<MutableList<Int>>, i: Int, j: Int): String {
    if (i == j) {
        return "A" + _numToStr(i)
    } else {
        var left: String = optimal_parenthesization(s, i, ((s[i]!!) as MutableList<Int>)[j]!!)
        var right: String = optimal_parenthesization(s, ((s[i]!!) as MutableList<Int>)[j]!! + 1, j)
        return ((("( " + left) + " ") + right) + " )"
    }
}

fun user_main(): Unit {
    var arr: MutableList<Int> = mutableListOf(30, 35, 15, 5, 10, 20, 25)
    var n: Int = (arr.size).toInt()
    var res: MatrixChainResult = matrix_chain_order(arr)
    var m: MutableList<MutableList<Int>> = res.matrix
    var s: MutableList<MutableList<Int>> = res.solution
    println("No. of Operation required: " + _numToStr(((m[1]!!) as MutableList<Int>)[n - 1]!!))
    var seq: String = optimal_parenthesization(s, 1, n - 1)
    println(seq)
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
