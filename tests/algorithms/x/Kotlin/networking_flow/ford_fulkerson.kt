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

var INF: Int = (1000000000).toInt()
var graph: MutableList<MutableList<Int>> = mutableListOf(mutableListOf(0, 16, 13, 0, 0, 0), mutableListOf(0, 0, 10, 12, 0, 0), mutableListOf(0, 4, 0, 0, 14, 0), mutableListOf(0, 0, 9, 0, 0, 20), mutableListOf(0, 0, 0, 7, 0, 4), mutableListOf(0, 0, 0, 0, 0, 0))
fun breadth_first_search(graph: MutableList<MutableList<Int>>, source: Int, sink: Int, parent: MutableList<Int>): Boolean {
    var visited: MutableList<Boolean> = mutableListOf<Boolean>()
    var i: Int = (0).toInt()
    while (i < graph.size) {
        visited = run { val _tmp = visited.toMutableList(); _tmp.add(false); _tmp }
        i = i + 1
    }
    var queue: MutableList<Int> = mutableListOf<Int>()
    queue = run { val _tmp = queue.toMutableList(); _tmp.add(source); _tmp }
    _listSet(visited, source, true)
    var head: Int = (0).toInt()
    while (head < queue.size) {
        var u: Int = (queue[head]!!).toInt()
        head = head + 1
        var row: MutableList<Int> = graph[u]!!
        var ind: Int = (0).toInt()
        while (ind < row.size) {
            var capacity: Int = (row[ind]!!).toInt()
            if ((visited[ind]!! == false) && (capacity > 0)) {
                queue = run { val _tmp = queue.toMutableList(); _tmp.add(ind); _tmp }
                _listSet(visited, ind, true)
                _listSet(parent, ind, u)
            }
            ind = ind + 1
        }
    }
    return visited[sink]!!
}

fun ford_fulkerson(graph: MutableList<MutableList<Int>>, source: Int, sink: Int): Int {
    var parent: MutableList<Int> = mutableListOf<Int>()
    var i: Int = (0).toInt()
    while (i < graph.size) {
        parent = run { val _tmp = parent.toMutableList(); _tmp.add(0 - 1); _tmp }
        i = i + 1
    }
    var max_flow: Int = (0).toInt()
    while ((breadth_first_search(graph, source, sink, parent)) as Boolean) {
        var path_flow: Int = (INF).toInt()
        var s: Int = (sink).toInt()
        while (s != source) {
            var prev: Int = (parent[s]!!).toInt()
            var cap: Int = (((graph[prev]!!) as MutableList<Int>)[s]!!).toInt()
            if (cap < path_flow) {
                path_flow = cap
            }
            s = prev
        }
        max_flow = max_flow + path_flow
        var v: Int = (sink).toInt()
        while (v != source) {
            var u: Int = (parent[v]!!).toInt()
            _listSet(graph[u]!!, v, ((graph[u]!!) as MutableList<Int>)[v]!! - path_flow)
            _listSet(graph[v]!!, u, ((graph[v]!!) as MutableList<Int>)[u]!! + path_flow)
            v = u
        }
        var j: Int = (0).toInt()
        while (j < parent.size) {
            _listSet(parent, j, 0 - 1)
            j = j + 1
        }
    }
    return max_flow
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println(_numToStr(ford_fulkerson(graph, 0, 5)))
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
