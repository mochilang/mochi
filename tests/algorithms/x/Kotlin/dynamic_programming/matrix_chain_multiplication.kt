import java.math.BigInteger

val _dataDir = "/workspace/mochi/tests/github/TheAlgorithms/Mochi/dynamic_programming"

fun <T> _listSet(lst: MutableList<T>, idx: Int, v: T) { while (lst.size <= idx) lst.add(v); lst[idx] = v }

fun expect(cond: Boolean) { if (!cond) throw RuntimeException("expect failed") }

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

var INF: Int = (1000000000).toInt()
fun matrix_chain_multiply(arr: MutableList<Int>): Int {
    if (arr.size < 2) {
        return 0
    }
    var n: Int = (arr.size).toInt()
    var dp: MutableList<MutableList<Int>> = mutableListOf<MutableList<Int>>()
    var i: Int = (0).toInt()
    while (i < n) {
        var row: MutableList<Int> = mutableListOf<Int>()
        var j: Int = (0).toInt()
        while (j < n) {
            row = run { val _tmp = row.toMutableList(); _tmp.add(INF); _tmp }
            j = j + 1
        }
        dp = run { val _tmp = dp.toMutableList(); _tmp.add(row); _tmp }
        i = i + 1
    }
    i = n - 1
    while (i > 0) {
        var j: Int = (i).toInt()
        while (j < n) {
            if (i == j) {
                _listSet(dp[i]!!, j, 0)
            } else {
                var k: Int = (i).toInt()
                while (k < j) {
                    var cost: Int = ((((dp[i]!!) as MutableList<Int>)[k]!! + ((dp[k + 1]!!) as MutableList<Int>)[j]!!) + ((arr[i - 1]!! * arr[k]!!) * arr[j]!!)).toInt()
                    if (cost < ((dp[i]!!) as MutableList<Int>)[j]!!) {
                        _listSet(dp[i]!!, j, cost)
                    }
                    k = k + 1
                }
            }
            j = j + 1
        }
        i = i - 1
    }
    return ((dp[1]!!) as MutableList<Int>)[n - 1]!!
}

fun test_example(): Unit {
    expect(matrix_chain_multiply(mutableListOf(1, 2, 3, 4, 3)) == 30)
}

fun test_single_matrix(): Unit {
    expect(matrix_chain_multiply(mutableListOf(10)) == 0)
}

fun test_two_matrices(): Unit {
    expect(matrix_chain_multiply(mutableListOf(10, 20)) == 0)
}

fun test_cost_calculation(): Unit {
    expect(matrix_chain_multiply(mutableListOf(19, 2, 19)) == 722)
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        test_example()
        test_single_matrix()
        test_two_matrices()
        test_cost_calculation()
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
