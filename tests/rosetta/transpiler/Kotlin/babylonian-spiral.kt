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

fun push(h: MutableList<MutableMap<String, Int>>, it: MutableMap<String, Int>): MutableList<MutableMap<String, Int>> {
    var h: MutableList<MutableMap<String, Int>> = h
    h = run { val _tmp = h.toMutableList(); _tmp.add(it); _tmp } as MutableList<MutableMap<String, Int>>
    var i: BigInteger = (h.size - 1).toBigInteger()
    while ((i.compareTo((0).toBigInteger()) > 0) && (((h[(i.subtract((1).toBigInteger())).toInt()]!!) as MutableMap<String, Int>)["s"] as Int > ((h[(i).toInt()]!!) as MutableMap<String, Int>)["s"] as Int)) {
        var tmp: MutableMap<String, Int> = h[(i.subtract((1).toBigInteger())).toInt()]!!
        h[(i.subtract((1).toBigInteger())).toInt()] = h[(i).toInt()]!!
        h[(i).toInt()] = tmp
        i = i.subtract((1).toBigInteger())
    }
    return h
}

fun step(h: MutableList<MutableMap<String, Int>>, nv: Int, dir: MutableList<Int>): MutableMap<String, Any?> {
    var nv: Int = nv
    var h: MutableList<MutableMap<String, Int>> = h
    while ((h.size == 0) || ((nv * nv) <= ((h[0]!!) as MutableMap<String, Int>)["s"] as Int)) {
        h = push(h, mutableMapOf<String, Int>("s" to (nv * nv), "a" to (nv), "b" to (0)))
        nv = nv + 1
    }
    var s: Int = ((h[0]!!) as MutableMap<String, Int>)["s"] as Int
    var v: MutableList<MutableList<Int>> = mutableListOf<MutableList<Int>>()
    while ((h.size > 0) && (((h[0]!!) as MutableMap<String, Int>)["s"] as Int == s)) {
        var it: MutableMap<String, Int> = h[0]!!
        h = h.subList(1, h.size)
        v = run { val _tmp = v.toMutableList(); _tmp.add(mutableListOf((it)["a"] as Int, (it)["b"] as Int)); _tmp } as MutableList<MutableList<Int>>
        if ((it)["a"] as Int > (it)["b"] as Int) {
            h = push(h, mutableMapOf<String, Int>("s" to (((it)["a"] as Int * (it)["a"] as Int) + (((it)["b"] as Int + 1) * ((it)["b"] as Int + 1))), "a" to ((it)["a"] as Int), "b" to ((it)["b"] as Int + 1)))
        }
    }
    var list: MutableList<MutableList<Int>> = mutableListOf<MutableList<Int>>()
    for (p in v) {
        list = run { val _tmp = list.toMutableList(); _tmp.add(p); _tmp } as MutableList<MutableList<Int>>
    }
    var temp: MutableList<MutableList<Int>> = list
    for (p in temp) {
        if (p[0]!! != p[1]!!) {
            list = run { val _tmp = list.toMutableList(); _tmp.add(mutableListOf(p[1]!!, p[0]!!)); _tmp } as MutableList<MutableList<Int>>
        }
    }
    temp = list
    for (p in temp) {
        if (p[1]!! != 0) {
            list = run { val _tmp = list.toMutableList(); _tmp.add(mutableListOf(p[0]!!, 0 - p[1]!!)); _tmp } as MutableList<MutableList<Int>>
        }
    }
    temp = list
    for (p in temp) {
        if (p[0]!! != 0) {
            list = run { val _tmp = list.toMutableList(); _tmp.add(mutableListOf(0 - p[0]!!, p[1]!!)); _tmp } as MutableList<MutableList<Int>>
        }
    }
    var bestDot: Int = 0 - 999999999
    var best: MutableList<Int> = dir
    for (p in list) {
        var cross: BigInteger = ((p[0]!! * dir[1]!!) - (p[1]!! * dir[0]!!)).toBigInteger()
        if (cross.compareTo((0).toBigInteger()) >= 0) {
            var dot: BigInteger = ((p[0]!! * dir[0]!!) + (p[1]!! * dir[1]!!)).toBigInteger()
            if (dot.compareTo((bestDot).toBigInteger()) > 0) {
                bestDot = dot.toInt()
                best = p
            }
        }
    }
    return mutableMapOf<String, Any?>("d" to (best), "heap" to (h), "n" to (nv))
}

fun positions(n: Int): MutableList<MutableList<Int>> {
    var pos: MutableList<MutableList<Int>> = mutableListOf<MutableList<Int>>()
    var x: Int = 0
    var y: Int = 0
    var dir: MutableList<Int> = mutableListOf(0, 1)
    var heap: MutableList<MutableMap<String, Int>> = mutableListOf<MutableMap<String, Int>>()
    var nv: Int = 1
    var i: Int = 0
    while (i < n) {
        pos = run { val _tmp = pos.toMutableList(); _tmp.add(mutableListOf(x, y)); _tmp } as MutableList<MutableList<Int>>
        var st: MutableMap<String, Any?> = step(heap, nv, dir)
        dir = ((st)["d"] as Any?) as MutableList<Int>
        heap = ((st)["heap"] as Any?) as MutableList<MutableMap<String, Int>>
        nv = (st)["n"] as Int
        x = x + dir[0]!!
        y = y + dir[1]!!
        i = i + 1
    }
    return pos
}

fun pad(s: String, w: Int): String {
    var r: String = s
    while (r.length < w) {
        r = r + " "
    }
    return r
}

fun user_main(): Unit {
    var pts: MutableList<MutableList<Int>> = positions(40)
    println("The first 40 Babylonian spiral points are:")
    var line: String = ""
    var i: Int = 0
    while (i < pts.size) {
        var p: MutableList<Int> = pts[i]!!
        var s: String = pad(((("(" + (p[0]!!).toString()) + ", ") + (p[1]!!).toString()) + ")", 10)
        line = line + s
        if ((Math.floorMod((i + 1), 10)) == 0) {
            println(line)
            line = ""
        }
        i = i + 1
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
