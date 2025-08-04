import java.math.BigInteger

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

var INF: Int = 1000000000
var graph: MutableMap<String, MutableMap<String, Int>> = mutableMapOf<String, MutableMap<String, Int>>()
fun addEdge(u: String, v: String, w: Int): Unit {
    if (!(u in graph)) {
        (graph)[u] = mutableMapOf<String, Int>()
    }
    ((graph)[u]!!)[v] = w
    if (!(v in graph)) {
        (graph)[v] = mutableMapOf<String, Int>()
    }
}

fun removeAt(xs: MutableList<String>, idx: Int): MutableList<String> {
    var out: MutableList<String> = mutableListOf<String>()
    var i: Int = 0
    for (x in xs) {
        if (i != idx) {
            out = run { val _tmp = out.toMutableList(); _tmp.add(x); _tmp }
        }
        i = i + 1
    }
    return out
}

fun dijkstra(source: String): MutableMap<String, Any?> {
    var dist: MutableMap<String, Int> = mutableMapOf<String, Int>()
    var prev: MutableMap<String, String> = mutableMapOf<String, String>()
    for (v in graph.keys) {
        (dist)[v] = INF
        (prev)[v] = ""
    }
    (dist)[source] = 0
    var q: MutableList<String> = mutableListOf<String>()
    for (v in graph.keys) {
        q = run { val _tmp = q.toMutableList(); _tmp.add(v); _tmp }
    }
    while (q.size > 0) {
        var bestIdx: Int = 0
        var u: String = q[0]!!
        var i: Int = 1
        while (i < q.size) {
            var v: String = q[i]!!
            if ((dist)[v]!! < (dist)[u]!!) {
                u = v
                bestIdx = i
            }
            i = i + 1
        }
        q = removeAt(q, bestIdx)
        for (v in (graph)[u]!!.keys) {
            var alt: BigInteger = ((dist)[u]!! + ((((graph)[u]!!) as MutableMap<String, Int>))[v]!!).toBigInteger()
            if (alt.compareTo(((dist)[v]!!).toBigInteger()) < 0) {
                (dist)[v] = (alt.toInt())
                (prev)[v] = u
            }
        }
    }
    return mutableMapOf<String, Any?>("dist" to (dist), "prev" to (prev))
}

fun path(prev: MutableMap<String, String>, v: String): String {
    var s: String = v
    var cur: String = v
    while ((prev)[cur]!! != "") {
        cur = (prev)[cur]!!
        s = cur + s
    }
    return s
}

fun user_main(): Unit {
    addEdge("a", "b", 7)
    addEdge("a", "c", 9)
    addEdge("a", "f", 14)
    addEdge("b", "c", 10)
    addEdge("b", "d", 15)
    addEdge("c", "d", 11)
    addEdge("c", "f", 2)
    addEdge("d", "e", 6)
    addEdge("e", "f", 9)
    var res: MutableMap<String, Any?> = dijkstra("a")
    var dist: MutableMap<String, Int> = (((res)["dist"]!!) as MutableMap<String, Int>)
    var prev: MutableMap<String, String> = (((res)["prev"]!!) as MutableMap<String, String>)
    println((("Distance to e: " + ((dist)["e"]!!).toString()) + ", Path: ") + path(prev, "e"))
    println((("Distance to f: " + ((dist)["f"]!!).toString()) + ", Path: ") + path(prev, "f"))
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
