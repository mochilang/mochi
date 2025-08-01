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

data class FWResult(var dist: MutableList<MutableList<Int>>, var next: MutableList<MutableList<Int>>)
var INF: Int = 1000000
var n: Int = 4
var g: MutableList<MutableList<Int>> = mutableListOf<MutableList<Int>>()
fun floydWarshall(graph: MutableList<MutableList<Int>>): FWResult {
    var n: Int = graph.size
    var dist: MutableList<MutableList<Int>> = mutableListOf<MutableList<Int>>()
    var next: MutableList<MutableList<Int>> = mutableListOf<MutableList<Int>>()
    var i: Int = 0
    while (i < n) {
        var drow: MutableList<Int> = mutableListOf<Int>()
        var nrow: MutableList<Int> = mutableListOf<Int>()
        var j: Int = 0
        while (j < n) {
            drow = run { val _tmp = drow.toMutableList(); _tmp.add(((graph[i]!!) as MutableList<Int>)[j]!!); _tmp } as MutableList<Int>
            if ((((graph[i]!!) as MutableList<Int>)[j]!! < INF) && (i != j)) {
                nrow = run { val _tmp = nrow.toMutableList(); _tmp.add(j); _tmp } as MutableList<Int>
            } else {
                nrow = run { val _tmp = nrow.toMutableList(); _tmp.add(0 - 1); _tmp } as MutableList<Int>
            }
            j = j + 1
        }
        dist = run { val _tmp = dist.toMutableList(); _tmp.add(drow); _tmp } as MutableList<MutableList<Int>>
        next = run { val _tmp = next.toMutableList(); _tmp.add(nrow); _tmp } as MutableList<MutableList<Int>>
        i = i + 1
    }
    var k: Int = 0
    while (k < n) {
        var i: Int = 0
        while (i < n) {
            var j: Int = 0
            while (j < n) {
                if ((((dist[i]!!) as MutableList<Int>)[k]!! < INF) && (((dist[k]!!) as MutableList<Int>)[j]!! < INF)) {
                    var alt: BigInteger = (((dist[i]!!) as MutableList<Int>)[k]!! + ((dist[k]!!) as MutableList<Int>)[j]!!).toBigInteger()
                    if (alt.compareTo(((dist[i]!!) as MutableList<Int>)[j]!!.toBigInteger()) < 0) {
                        ((dist[i]!!)[j]) = alt.toInt()
                        ((next[i]!!)[j]) = ((next[i]!!) as MutableList<Int>)[k]!!
                    }
                }
                j = j + 1
            }
            i = i + 1
        }
        k = k + 1
    }
    return FWResult(dist = dist, next = next)
}

fun path(u: Int, v: Int, next: MutableList<MutableList<Int>>): MutableList<Int> {
    if (((next[u]!!) as MutableList<Int>)[v]!! < 0) {
        return mutableListOf<Int>()
    }
    var p: MutableList<Int> = mutableListOf(u)
    var x: Int = u
    while (x != v) {
        x = ((next[x]!!) as MutableList<Int>)[v]!!
        p = run { val _tmp = p.toMutableList(); _tmp.add(x); _tmp } as MutableList<Int>
    }
    return p
}

fun pathStr(p: MutableList<Int>): String {
    var s: String = ""
    var i: Int = 0
    while (i < p.size) {
        s = s + (p[i]!! + 1).toString()
        if (i < (p.size - 1)) {
            s = s + " -> "
        }
        i = i + 1
    }
    return s
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        for (i in 0 until n) {
            var row: MutableList<Int> = mutableListOf<Int>()
            for (j in 0 until n) {
                if (i == j) {
                    row = run { val _tmp = row.toMutableList(); _tmp.add(0); _tmp } as MutableList<Int>
                } else {
                    row = run { val _tmp = row.toMutableList(); _tmp.add(INF); _tmp } as MutableList<Int>
                }
            }
            g = run { val _tmp = g.toMutableList(); _tmp.add(row); _tmp } as MutableList<MutableList<Int>>
        }
        ((g[0]!!)[2]) = 0 - 2
        ((g[2]!!)[3]) = 2
        ((g[3]!!)[1]) = 0 - 1
        ((g[1]!!)[0]) = 4
        ((g[1]!!)[2]) = 3
        var res: FWResult = floydWarshall(g)
        println("pair\tdist\tpath")
        var i: Int = 0
        while (i < n) {
            var j: Int = 0
            while (j < n) {
                if (i != j) {
                    var p: MutableList<Int> = path(i, j, res.next)
                    println(((((((i + 1).toString() + " -> ") + (j + 1).toString()) + "\t") + ((((res.dist)[i]!!) as MutableList<Int>)[j]!!).toString()) + "\t") + pathStr(p))
                }
                j = j + 1
            }
            i = i + 1
        }
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
