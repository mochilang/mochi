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

var test_graph: MutableList<MutableList<Int>> = mutableListOf(mutableListOf(0, 16, 13, 0, 0, 0), mutableListOf(0, 0, 10, 12, 0, 0), mutableListOf(0, 4, 0, 0, 14, 0), mutableListOf(0, 0, 9, 0, 0, 20), mutableListOf(0, 0, 0, 7, 0, 4), mutableListOf(0, 0, 0, 0, 0, 0))
var result: MutableList<MutableList<Int>> = mincut(test_graph, 0, 5)
fun bfs(graph: MutableList<MutableList<Int>>, s: Int, t: Int, parent: MutableList<Int>): Boolean {
    var visited: MutableList<Boolean> = mutableListOf<Boolean>()
    var i: Int = (0).toInt()
    while (i < graph.size) {
        visited = run { val _tmp = visited.toMutableList(); _tmp.add(false); _tmp }
        i = i + 1
    }
    var queue: MutableList<Int> = mutableListOf(s)
    var head: Int = (0).toInt()
    _listSet(visited, s, true)
    while (head < queue.size) {
        var u: Int = (queue[head]!!).toInt()
        head = head + 1
        var ind: Int = (0).toInt()
        while (ind < (graph[u]!!).size) {
            if ((visited[ind]!! == false) && (((graph[u]!!) as MutableList<Int>)[ind]!! > 0)) {
                queue = run { val _tmp = queue.toMutableList(); _tmp.add(ind); _tmp }
                _listSet(visited, ind, true)
                _listSet(parent, ind, u)
            }
            ind = ind + 1
        }
    }
    return visited[t]!!
}

fun mincut(graph: MutableList<MutableList<Int>>, source: Int, sink: Int): MutableList<MutableList<Int>> {
    var g: MutableList<MutableList<Int>> = graph
    var parent: MutableList<Int> = mutableListOf<Int>()
    var i: Int = (0).toInt()
    while (i < g.size) {
        parent = run { val _tmp = parent.toMutableList(); _tmp.add(0 - 1); _tmp }
        i = i + 1
    }
    var temp: MutableList<MutableList<Int>> = mutableListOf<MutableList<Int>>()
    i = 0
    while (i < g.size) {
        var row: MutableList<Int> = mutableListOf<Int>()
        var j: Int = (0).toInt()
        while (j < (g[i]!!).size) {
            row = run { val _tmp = row.toMutableList(); _tmp.add(((g[i]!!) as MutableList<Int>)[j]!!); _tmp }
            j = j + 1
        }
        temp = run { val _tmp = temp.toMutableList(); _tmp.add(row); _tmp }
        i = i + 1
    }
    while ((bfs(g, source, sink, parent)) as Boolean) {
        var path_flow: Int = (1000000000).toInt()
        var s: Int = (sink).toInt()
        while (s != source) {
            var p: Int = (parent[s]!!).toInt()
            var cap: Int = (((g[p]!!) as MutableList<Int>)[s]!!).toInt()
            if (cap < path_flow) {
                path_flow = cap
            }
            s = p
        }
        var v: Int = (sink).toInt()
        while (v != source) {
            var u: Int = (parent[v]!!).toInt()
            _listSet(g[u]!!, v, ((g[u]!!) as MutableList<Int>)[v]!! - path_flow)
            _listSet(g[v]!!, u, ((g[v]!!) as MutableList<Int>)[u]!! + path_flow)
            v = u
        }
    }
    var res: MutableList<MutableList<Int>> = mutableListOf<MutableList<Int>>()
    i = 0
    while (i < g.size) {
        var j: Int = (0).toInt()
        while (j < (g[0]!!).size) {
            if ((((g[i]!!) as MutableList<Int>)[j]!! == 0) && (((temp[i]!!) as MutableList<Int>)[j]!! > 0)) {
                res = run { val _tmp = res.toMutableList(); _tmp.add(mutableListOf(i, j)); _tmp }
            }
            j = j + 1
        }
        i = i + 1
    }
    return res
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println(result.toString())
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
