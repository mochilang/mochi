import java.math.BigInteger

fun <T> _listSet(lst: MutableList<T>, idx: Int, v: T) { while (lst.size <= idx) lst.add(v); lst[idx] = v }

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

data class KnapsackResult(var value: Int = 0, var subset: MutableList<Int> = mutableListOf<Int>())
var f: MutableList<MutableList<Int>> = mutableListOf<MutableList<Int>>()
var val_list: MutableList<Int> = mutableListOf(3, 2, 4, 4)
var wt_list: MutableList<Int> = mutableListOf(4, 3, 2, 3)
var n: Int = (4).toInt()
var w_cap: Int = (6).toInt()
fun max_int(a: Int, b: Int): Int {
    if (a > b) {
        return a
    } else {
        return b
    }
}

fun init_f(n: Int, w: Int): MutableList<MutableList<Int>> {
    var table: MutableList<MutableList<Int>> = mutableListOf<MutableList<Int>>()
    var i: Int = (0).toInt()
    while (i <= n) {
        var row: MutableList<Int> = mutableListOf<Int>()
        var j: Int = (0).toInt()
        while (j <= w) {
            if ((i == 0) || (j == 0)) {
                row = run { val _tmp = row.toMutableList(); _tmp.add(0); _tmp }
            } else {
                row = run { val _tmp = row.toMutableList(); _tmp.add(0 - 1); _tmp }
            }
            j = j + 1
        }
        table = run { val _tmp = table.toMutableList(); _tmp.add(row); _tmp }
        i = i + 1
    }
    return table
}

fun mf_knapsack(i: Int, wt: MutableList<Int>, _val: MutableList<Int>, j: Int): Int {
    if ((((f[i]!!) as MutableList<Int>))[j]!! < 0) {
        if (j < wt[i - 1]!!) {
            _listSet(f[i]!!, j, mf_knapsack(i - 1, wt, _val, j))
        } else {
            var without_item: Int = (mf_knapsack(i - 1, wt, _val, j)).toInt()
            var with_item: Int = (mf_knapsack(i - 1, wt, _val, j - wt[i - 1]!!) + _val[i - 1]!!).toInt()
            _listSet(f[i]!!, j, max_int(without_item, with_item))
        }
    }
    return (((f[i]!!) as MutableList<Int>))[j]!!
}

fun create_matrix(rows: Int, cols: Int): MutableList<MutableList<Int>> {
    var matrix: MutableList<MutableList<Int>> = mutableListOf<MutableList<Int>>()
    var i: Int = (0).toInt()
    while (i <= rows) {
        var row: MutableList<Int> = mutableListOf<Int>()
        var j: Int = (0).toInt()
        while (j <= cols) {
            row = run { val _tmp = row.toMutableList(); _tmp.add(0); _tmp }
            j = j + 1
        }
        matrix = run { val _tmp = matrix.toMutableList(); _tmp.add(row); _tmp }
        i = i + 1
    }
    return matrix
}

fun knapsack(w: Int, wt: MutableList<Int>, _val: MutableList<Int>, n: Int): MutableList<MutableList<Int>> {
    var dp: MutableList<MutableList<Int>> = create_matrix(n, w)
    var i: Int = (1).toInt()
    while (i <= n) {
        var w_: Int = (1).toInt()
        while (w_ <= w) {
            if (wt[i - 1]!! <= w_) {
                var include: Int = (_val[i - 1]!! + (((dp[i - 1]!!) as MutableList<Int>))[w_ - wt[i - 1]!!]!!).toInt()
                var exclude: Int = ((((dp[i - 1]!!) as MutableList<Int>))[w_]!!).toInt()
                _listSet(dp[i]!!, w_, max_int(include, exclude))
            } else {
                _listSet(dp[i]!!, w_, (((dp[i - 1]!!) as MutableList<Int>))[w_]!!)
            }
            w_ = w_ + 1
        }
        i = i + 1
    }
    return dp
}

fun construct_solution(dp: MutableList<MutableList<Int>>, wt: MutableList<Int>, i: Int, j: Int, optimal_set: MutableList<Int>): MutableList<Int> {
    if ((i > 0) && (j > 0)) {
        if ((((dp[i - 1]!!) as MutableList<Int>))[j]!! == (((dp[i]!!) as MutableList<Int>))[j]!!) {
            return construct_solution(dp, wt, i - 1, j, optimal_set)
        } else {
            var with_prev: MutableList<Int> = construct_solution(dp, wt, i - 1, j - wt[i - 1]!!, optimal_set)
            return run { val _tmp = with_prev.toMutableList(); _tmp.add(i); _tmp }
        }
    }
    return optimal_set
}

fun knapsack_with_example_solution(w: Int, wt: MutableList<Int>, _val: MutableList<Int>): KnapsackResult {
    var num_items: Int = (wt.size).toInt()
    var dp_table: MutableList<MutableList<Int>> = knapsack(w, wt, _val, num_items)
    var optimal_val: Int = ((((dp_table[num_items]!!) as MutableList<Int>))[w]!!).toInt()
    var subset: MutableList<Int> = construct_solution(dp_table, wt, num_items, w, mutableListOf<Int>())
    return KnapsackResult(value = optimal_val, subset = subset)
}

fun format_set(xs: MutableList<Int>): String {
    var res: String = "{"
    var i: Int = (0).toInt()
    while (i < xs.size) {
        res = res + (xs[i]!!).toString()
        if ((i + 1) < xs.size) {
            res = res + ", "
        }
        i = i + 1
    }
    res = res + "}"
    return res
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        f = init_f(n, w_cap)
        var dp_table: MutableList<MutableList<Int>> = knapsack(w_cap, wt_list, val_list, n)
        var optimal_solution: Int = ((((dp_table[n]!!) as MutableList<Int>))[w_cap]!!).toInt()
        println(optimal_solution)
        println(mf_knapsack(n, wt_list, val_list, w_cap))
        var example: KnapsackResult = knapsack_with_example_solution(w_cap, wt_list, val_list)
        println("optimal_value = " + example.value.toString())
        println("An optimal subset corresponding to the optimal value " + format_set(example.subset))
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
