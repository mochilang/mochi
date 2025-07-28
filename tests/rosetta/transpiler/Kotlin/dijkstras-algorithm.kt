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

val INF: Int = 1000000000
var graph: MutableMap<String, MutableMap<String, Int>> = mutableMapOf<Any?, Any?>() as MutableMap<String, MutableMap<String, Int>>
fun addEdge(u: String, v: String, w: Int): Unit {
    if (!((u in graph) as Boolean)) {
        (graph)[u] as MutableMap<String, Int> = mutableMapOf<Any?, Any?>() as MutableMap<String, Int>
    }
    ((graph)[u] as MutableMap<String, Int>)[v] as Int = w
    if (!((v in graph) as Boolean)) {
        (graph)[v] as MutableMap<String, Int> = mutableMapOf<Any?, Any?>() as MutableMap<String, Int>
    }
}

fun removeAt(xs: MutableList<String>, idx: Int): MutableList<String> {
    var out: MutableList<String> = mutableListOf<String>()
    var i: Int = 0
    for (x in xs) {
        if (i != idx) {
            out = run { val _tmp = out.toMutableList(); _tmp.add(x); _tmp } as MutableList<String>
        }
        i = i + 1
    }
    return out
}

fun dijkstra(source: String): MutableMap<String, Any?> {
    var dist: MutableMap<String, Int> = mutableMapOf<Any?, Any?>() as MutableMap<String, Int>
    var prev: MutableMap<String, String> = mutableMapOf<Any?, Any?>() as MutableMap<String, String>
    for (v in graph.keys) {
        (dist)[v] as Int = INF
        (prev)[v] as String = ""
    }
    (dist)[source] as Int = 0
    var q: MutableList<String> = mutableListOf<String>()
    for (v in graph.keys) {
        q = run { val _tmp = q.toMutableList(); _tmp.add(v as String); _tmp } as MutableList<String>
    }
    while (q.size > 0) {
        var bestIdx: Int = 0
        var u: String = q[0]
        var i: Int = 1
        while (i < q.size) {
            val v: String = q[i]
            if ((dist)[v] as Int < (dist)[u] as Int) {
                u = v
                bestIdx = i
            }
            i = i + 1
        }
        q = removeAt(q, bestIdx)
        for (v in (graph)[u] as MutableMap<String, Int>) {
            val alt: BigInteger = ((dist)[u] as Int + ((graph)[u] as MutableMap<String, Int>)[v] as Int).toBigInteger()
            if (alt.compareTo((dist)[v] as Int.toBigInteger()) < 0) {
                (dist)[v] as Int = alt.toInt()
                (prev)[v] as String = u
            }
        }
    }
    return mutableMapOf<String, Any?>("dist" to (dist), "prev" to (prev))
}

fun path(prev: MutableMap<String, String>, v: String): String {
    var s: String = v
    var cur: String = v
    while ((prev)[cur] as String != "") {
        cur = (prev)[cur] as String
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
    val res: MutableMap<String, Any?> = dijkstra("a")
    val dist: MutableMap<String, Int> = ((res)["dist"] as Any?) as MutableMap<String, Int>
    val prev: MutableMap<String, String> = ((res)["prev"] as Any?) as MutableMap<String, String>
    println((("Distance to e: " + ((dist)["e"] as Int).toString()) + ", Path: ") + path(prev, "e"))
    println((("Distance to f: " + ((dist)["f"] as Int).toString()) + ", Path: ") + path(prev, "f"))
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
