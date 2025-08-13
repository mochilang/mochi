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

data class Graph(var n: Int = 0, var dp: MutableList<MutableList<Int>> = mutableListOf<MutableList<Int>>())
var INF: Int = (1000000000).toInt()
var graph: Graph = new_graph(5)
fun new_graph(n: Int): Graph {
    var dp: MutableList<MutableList<Int>> = mutableListOf<MutableList<Int>>()
    var i: Int = (0).toInt()
    while (i < n) {
        var row: MutableList<Int> = mutableListOf<Int>()
        var j: Int = (0).toInt()
        while (j < n) {
            if (i == j) {
                row = run { val _tmp = row.toMutableList(); _tmp.add(0); _tmp }
            } else {
                row = run { val _tmp = row.toMutableList(); _tmp.add(INF); _tmp }
            }
            j = j + 1
        }
        dp = run { val _tmp = dp.toMutableList(); _tmp.add(row); _tmp }
        i = i + 1
    }
    return Graph(n = n, dp = dp)
}

fun add_edge(g: Graph, u: Int, v: Int, w: Int): Unit {
    var dp: MutableList<MutableList<Int>> = g.dp
    var row: MutableList<Int> = dp[u]!!
    _listSet(row, v, w)
    _listSet(dp, u, row)
    g.dp = dp
}

fun floyd_warshall(g: Graph): Unit {
    var dp: MutableList<MutableList<Int>> = g.dp
    var k: Int = (0).toInt()
    while (k < g.n) {
        var i: Int = (0).toInt()
        while (i < g.n) {
            var j: Int = (0).toInt()
            while (j < g.n) {
                var alt: Int = ((((dp[i]!!) as MutableList<Int>))[k]!! + (((dp[k]!!) as MutableList<Int>))[j]!!).toInt()
                var row: MutableList<Int> = dp[i]!!
                if (alt < row[j]!!) {
                    _listSet(row, j, alt)
                    _listSet(dp, i, row)
                }
                j = j + 1
            }
            i = i + 1
        }
        k = k + 1
    }
    g.dp = dp
}

fun show_min(g: Graph, u: Int, v: Int): Int {
    return ((((g.dp)[u]!!) as MutableList<Int>))[v]!!
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        add_edge(graph, 0, 2, 9)
        add_edge(graph, 0, 4, 10)
        add_edge(graph, 1, 3, 5)
        add_edge(graph, 2, 3, 7)
        add_edge(graph, 3, 0, 10)
        add_edge(graph, 3, 1, 2)
        add_edge(graph, 3, 2, 1)
        add_edge(graph, 3, 4, 6)
        add_edge(graph, 4, 1, 3)
        add_edge(graph, 4, 2, 4)
        add_edge(graph, 4, 3, 9)
        floyd_warshall(graph)
        println(show_min(graph, 1, 4).toString())
        println(show_min(graph, 0, 3).toString())
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
