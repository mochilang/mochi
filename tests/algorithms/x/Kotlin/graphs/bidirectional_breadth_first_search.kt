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

data class Node(var pos: String = "", var path: MutableList<String> = mutableListOf<String>())
var grid: MutableList<MutableList<Int>> = mutableListOf(mutableListOf(0, 0, 0, 0, 0, 0, 0), mutableListOf(0, 1, 0, 0, 0, 0, 0), mutableListOf(0, 0, 0, 0, 0, 0, 0), mutableListOf(0, 0, 1, 0, 0, 0, 0), mutableListOf(1, 0, 1, 0, 0, 0, 0), mutableListOf(0, 0, 0, 0, 0, 0, 0), mutableListOf(0, 0, 0, 0, 1, 0, 0))
var delta: MutableList<MutableList<Int>> = mutableListOf(mutableListOf(0 - 1, 0), mutableListOf(0, 0 - 1), mutableListOf(1, 0), mutableListOf(0, 1))
var start: String = key(0, 0)
var goal: String = key(grid.size - 1, (grid[0]!!).size - 1)
var path1: MutableList<String> = bfs(start, goal)
fun key(y: Int, x: Int): String {
    return (_numToStr(y) + ",") + _numToStr(x)
}

fun parse_int(s: String): Int {
    var value: Int = (0).toInt()
    var i: Int = (0).toInt()
    while (i < s.length) {
        var c: String = s[i].toString()
        value = (value * 10) + ((c.toBigInteger().toInt()))
        i = i + 1
    }
    return value
}

fun parse_key(k: String): MutableList<Int> {
    var idx: Int = (0).toInt()
    while ((idx < k.length) && (k.substring(idx, idx + 1) != ",")) {
        idx = idx + 1
    }
    var y: Int = (parse_int(k.substring(0, idx))).toInt()
    var x: Int = (parse_int(k.substring(idx + 1, k.length))).toInt()
    return mutableListOf(y, x)
}

fun neighbors(pos: String): MutableList<String> {
    var coords: MutableList<Int> = parse_key(pos)
    var y: Int = (coords[0]!!).toInt()
    var x: Int = (coords[1]!!).toInt()
    var res: MutableList<String> = mutableListOf<String>()
    var i: Int = (0).toInt()
    while (i < delta.size) {
        var ny: Int = (y + (((delta[i]!!) as MutableList<Int>))[0]!!).toInt()
        var nx: Int = (x + (((delta[i]!!) as MutableList<Int>))[1]!!).toInt()
        if ((((((ny >= 0) && (ny < grid.size) as Boolean)) && (nx >= 0) as Boolean)) && (nx < (grid[0]!!).size)) {
            if ((((grid[ny]!!) as MutableList<Int>))[nx]!! == 0) {
                res = run { val _tmp = res.toMutableList(); _tmp.add(key(ny, nx)); _tmp }
            }
        }
        i = i + 1
    }
    return res
}

fun reverse_list(lst: MutableList<String>): MutableList<String> {
    var res: MutableList<String> = mutableListOf<String>()
    var i: Int = (lst.size - 1).toInt()
    while (i >= 0) {
        res = run { val _tmp = res.toMutableList(); _tmp.add(lst[i]!!); _tmp }
        i = i - 1
    }
    return res
}

fun bfs(start: String, goal: String): MutableList<String> {
    var queue: MutableList<Node> = mutableListOf<Node>()
    queue = run { val _tmp = queue.toMutableList(); _tmp.add(Node(pos = start, path = mutableListOf(start))); _tmp }
    var head: Int = (0).toInt()
    var visited: MutableMap<String, Boolean> = (mutableMapOf<String, Boolean>("start" to (true)) as MutableMap<String, Boolean>)
    while (head < queue.size) {
        var node: Node = queue[head]!!
        head = head + 1
        if (node.pos == goal) {
            return node.path
        }
        var neigh: MutableList<String> = neighbors(node.pos)
        var i: Int = (0).toInt()
        while (i < neigh.size) {
            var npos: String = neigh[i]!!
            if (!(npos in visited)) {
                (visited)[npos] = true
                var new_path = run { val _tmp = (node.path).toMutableList(); _tmp.add(npos); _tmp }
                queue = run { val _tmp = queue.toMutableList(); _tmp.add(Node(pos = npos, path = new_path)); _tmp }
            }
            i = i + 1
        }
    }
    return mutableListOf<String>()
}

fun bidirectional_bfs(start: String, goal: String): MutableList<String> {
    var queue_f: MutableList<Node> = mutableListOf<Node>()
    var queue_b: MutableList<Node> = mutableListOf<Node>()
    queue_f = run { val _tmp = queue_f.toMutableList(); _tmp.add(Node(pos = start, path = mutableListOf(start))); _tmp }
    queue_b = run { val _tmp = queue_b.toMutableList(); _tmp.add(Node(pos = goal, path = mutableListOf(goal))); _tmp }
    var head_f: Int = (0).toInt()
    var head_b: Int = (0).toInt()
    var visited_f: MutableMap<String, MutableList<String>> = (mutableMapOf<String, MutableList<String>>("start" to (mutableListOf(start))) as MutableMap<String, MutableList<String>>)
    var visited_b: MutableMap<String, MutableList<String>> = (mutableMapOf<String, MutableList<String>>("goal" to (mutableListOf(goal))) as MutableMap<String, MutableList<String>>)
    while ((head_f < queue_f.size) && (head_b < queue_b.size)) {
        var node_f: Node = queue_f[head_f]!!
        head_f = head_f + 1
        var neigh_f: MutableList<String> = neighbors(node_f.pos)
        var i: Int = (0).toInt()
        while (i < neigh_f.size) {
            var npos: String = neigh_f[i]!!
            if (!(npos in visited_f)) {
                var new_path = run { val _tmp = (node_f.path).toMutableList(); _tmp.add(npos); _tmp }
                (visited_f)[npos] = (new_path as MutableList<String>)
                if (npos in visited_b) {
                    var rev: MutableList<String> = reverse_list((visited_b)[npos] as MutableList<String>)
                    var j: Int = (1).toInt()
                    while (j < rev.size) {
                        new_path = run { val _tmp = new_path.toMutableList(); _tmp.add(rev[j]!!); _tmp }
                        j = j + 1
                    }
                    return (new_path as MutableList<String>)
                }
                queue_f = run { val _tmp = queue_f.toMutableList(); _tmp.add(Node(pos = npos, path = new_path)); _tmp }
            }
            i = i + 1
        }
        var node_b: Node = queue_b[head_b]!!
        head_b = head_b + 1
        var neigh_b: MutableList<String> = neighbors(node_b.pos)
        var j: Int = (0).toInt()
        while (j < neigh_b.size) {
            var nposb: String = neigh_b[j]!!
            if (!(nposb in visited_b)) {
                var new_path_b = run { val _tmp = (node_b.path).toMutableList(); _tmp.add(nposb); _tmp }
                (visited_b)[nposb] = (new_path_b as MutableList<String>)
                if (nposb in visited_f) {
                    var path_f: MutableList<String>? = (((visited_f)[nposb] as MutableList<String>) as MutableList<String>?)
                    new_path_b = ((reverse_list(new_path_b)) as MutableList<Any?>)
                    var t: Int = (1).toInt()
                    while (t < new_path_b.size) {
                        path_f = run { val _tmp = path_f.toMutableList(); _tmp.add(((new_path_b[t] as Any?) as String)); _tmp }
                        t = t + 1
                    }
                    return path_f
                }
                queue_b = run { val _tmp = queue_b.toMutableList(); _tmp.add(Node(pos = nposb, path = new_path_b)); _tmp }
            }
            j = j + 1
        }
    }
    return mutableListOf(start)
}

fun path_to_string(path: MutableList<String>): String {
    if (path.size == 0) {
        return "[]"
    }
    var first: MutableList<Int> = parse_key(path[0]!!)
    var s: String = ((("[(" + _numToStr(first[0]!!)) + ", ") + _numToStr(first[1]!!)) + ")"
    var i: Int = (1).toInt()
    while (i < path.size) {
        var c: MutableList<Int> = parse_key(path[i]!!)
        s = ((((s + ", (") + _numToStr(c[0]!!)) + ", ") + _numToStr(c[1]!!)) + ")"
        i = i + 1
    }
    s = s + "]"
    return s
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println(path_to_string(path1))
        var path2: MutableList<String> = bidirectional_bfs(start, goal)
        println(path_to_string(path2))
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
