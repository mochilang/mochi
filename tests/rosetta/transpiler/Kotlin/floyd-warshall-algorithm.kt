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

fun user_main(): Unit {
    var INF: Int = 1000000000
    var n: Int = 4
    var dist: MutableList<MutableList<Int>> = mutableListOf<MutableList<Int>>()
    var next: MutableList<MutableList<Int>> = mutableListOf<MutableList<Int>>()
    var i: Int = 0
    while (i < n) {
        var row: MutableList<Int> = mutableListOf<Int>()
        var nrow: MutableList<Int> = mutableListOf<Int>()
        var j: Int = 0
        while (j < n) {
            if (i == j) {
                row = run { val _tmp = row.toMutableList(); _tmp.add(0); _tmp } as MutableList<Int>
            } else {
                row = run { val _tmp = row.toMutableList(); _tmp.add(INF); _tmp } as MutableList<Int>
            }
            nrow = run { val _tmp = nrow.toMutableList(); _tmp.add(0 - 1); _tmp } as MutableList<Int>
            j = j + 1
        }
        dist = run { val _tmp = dist.toMutableList(); _tmp.add(row); _tmp } as MutableList<MutableList<Int>>
        next = run { val _tmp = next.toMutableList(); _tmp.add(nrow); _tmp } as MutableList<MutableList<Int>>
        i = i + 1
    }
    ((dist[0]!!)[2]) = 0 - 2
    ((next[0]!!)[2]) = 2
    ((dist[2]!!)[3]) = 2
    ((next[2]!!)[3]) = 3
    ((dist[3]!!)[1]) = 0 - 1
    ((next[3]!!)[1]) = 1
    ((dist[1]!!)[0]) = 4
    ((next[1]!!)[0]) = 0
    ((dist[1]!!)[2]) = 3
    ((next[1]!!)[2]) = 2
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
    fun path(u: Int, v: Int): MutableList<Int> {
        var ui: Int = u - 1
        var vi: Int = v - 1
        if (((next[ui]!!) as MutableList<Int>)[vi]!! == (0 - 1)) {
            return mutableListOf<Int>()
        }
        var p: MutableList<Int> = mutableListOf(u)
        var cur: Int = ui
        while (cur != vi) {
            cur = ((next[cur]!!) as MutableList<Int>)[vi]!!
            p = run { val _tmp = p.toMutableList(); _tmp.add(cur + 1); _tmp } as MutableList<Int>
        }
        return p
    }

    fun pathStr(p: MutableList<Int>): String {
        var s: String = ""
        var first: Boolean = true
        var idx: Int = 0
        while (idx < p.size) {
            var x: Int = p[idx]!!
            if (!first) {
                s = s + " -> "
            }
            s = s + x.toString()
            first = false
            idx = idx + 1
        }
        return s
    }

    println("pair\tdist\tpath")
    var a: Int = 0
    while (a < n) {
        var b: Int = 0
        while (b < n) {
            if (a != b) {
                println(((((((a + 1).toString() + " -> ") + (b + 1).toString()) + "\t") + (((dist[a]!!) as MutableList<Int>)[b]!!).toString()) + "\t") + (pathStr((path(a + 1, b + 1)) as MutableList<Int>)).toString())
            }
            b = b + 1
        }
        a = a + 1
    }
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
