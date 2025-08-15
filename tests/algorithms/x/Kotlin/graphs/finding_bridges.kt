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

data class DfsResult(var id: Int = 0, var bridges: MutableList<MutableList<Int>> = mutableListOf<MutableList<Int>>())
fun dfs(graph: MutableMap<Int, MutableList<Int>>, at: Int, parent: Int, visited: MutableList<Boolean>, ids: MutableList<Int>, low: MutableList<Int>, id: Int, bridges: MutableList<MutableList<Int>>): DfsResult {
    _listSet(visited, at, true)
    _listSet(ids, at, id)
    _listSet(low, at, id)
    var current_id: Int = (id + 1).toInt()
    var res_bridges: MutableList<MutableList<Int>> = bridges
    for (to in (graph)[at] as MutableList<Int>) {
        if (to == parent) {
            continue
        } else {
            if (!((visited[to]!!) as? Boolean ?: false)) {
                var result: DfsResult = dfs(graph, to, at, visited, ids, low, current_id, res_bridges)
                current_id = result.id
                res_bridges = result.bridges
                if (low[at]!! > low[to]!!) {
                    _listSet(low, at, low[to]!!)
                }
                if (ids[at]!! < low[to]!!) {
                    var edge: MutableList<Int> = if (at < to) mutableListOf(at, to) else mutableListOf(to, at)
                    res_bridges = run { val _tmp = res_bridges.toMutableList(); _tmp.add(edge); _tmp }
                }
            } else {
                if (low[at]!! > ids[to]!!) {
                    _listSet(low, at, ids[to]!!)
                }
            }
        }
    }
    return DfsResult(id = current_id, bridges = res_bridges)
}

fun compute_bridges(graph: MutableMap<Int, MutableList<Int>>): MutableList<MutableList<Int>> {
    var n: Int = (graph.size).toInt()
    var visited: MutableList<Boolean> = mutableListOf<Boolean>()
    var ids: MutableList<Int> = mutableListOf<Int>()
    var low: MutableList<Int> = mutableListOf<Int>()
    var i: Int = (0).toInt()
    while (i < n) {
        visited = run { val _tmp = visited.toMutableList(); _tmp.add(false); _tmp }
        ids = run { val _tmp = ids.toMutableList(); _tmp.add(0); _tmp }
        low = run { val _tmp = low.toMutableList(); _tmp.add(0); _tmp }
        i = i + 1
    }
    var bridges: MutableList<MutableList<Int>> = mutableListOf<MutableList<Int>>()
    var id: Int = (0).toInt()
    i = 0
    while (i < n) {
        if (!((visited[i]!!) as? Boolean ?: false)) {
            var result: DfsResult = dfs(graph, i, 0 - 1, visited, ids, low, id, bridges)
            id = result.id
            bridges = result.bridges
        }
        i = i + 1
    }
    return bridges
}

fun get_demo_graph(index: Int): MutableMap<Int, MutableList<Int>> {
    if (index == 0) {
        return mutableMapOf<Int, MutableList<Int>>(0 to (mutableListOf(1, 2)), 1 to (mutableListOf(0, 2)), 2 to (mutableListOf(0, 1, 3, 5)), 3 to (mutableListOf(2, 4)), 4 to (mutableListOf(3)), 5 to (mutableListOf(2, 6, 8)), 6 to (mutableListOf(5, 7)), 7 to (mutableListOf(6, 8)), 8 to (mutableListOf(5, 7)))
    }
    if (index == 1) {
        return mutableMapOf<Int, MutableList<Int>>(0 to (mutableListOf(6)), 1 to (mutableListOf(9)), 2 to (mutableListOf(4, 5)), 3 to (mutableListOf(4)), 4 to (mutableListOf(2, 3)), 5 to (mutableListOf(2)), 6 to (mutableListOf(0, 7)), 7 to (mutableListOf(6)), 8 to (mutableListOf<Int>()), 9 to (mutableListOf(1)))
    }
    if (index == 2) {
        return mutableMapOf<Int, MutableList<Int>>(0 to (mutableListOf(4)), 1 to (mutableListOf(6)), 2 to (mutableListOf<Int>()), 3 to (mutableListOf(5, 6, 7)), 4 to (mutableListOf(0, 6)), 5 to (mutableListOf(3, 8, 9)), 6 to (mutableListOf(1, 3, 4, 7)), 7 to (mutableListOf(3, 6, 8, 9)), 8 to (mutableListOf(5, 7)), 9 to (mutableListOf(5, 7)))
    }
    return mutableMapOf<Int, MutableList<Int>>(0 to (mutableListOf(1, 3)), 1 to (mutableListOf(0, 2, 4)), 2 to (mutableListOf(1, 3, 4)), 3 to (mutableListOf(0, 2, 4)), 4 to (mutableListOf(1, 2, 3)))
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println(compute_bridges(get_demo_graph(0)))
        println(compute_bridges(get_demo_graph(1)))
        println(compute_bridges(get_demo_graph(2)))
        println(compute_bridges(get_demo_graph(3)))
        println(compute_bridges(mutableMapOf<Any?, Any?>()))
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
