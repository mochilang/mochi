import java.math.BigInteger

fun push(h: MutableList<MutableMap<String, Int>>, it: MutableMap<String, Int>): MutableList<MutableMap<String, Int>> {
    var h: MutableList<MutableMap<String, Int>> = h
    h = run { val _tmp = h.toMutableList(); _tmp.add(it); _tmp } as MutableList<MutableMap<String, Int>>
    var i: BigInteger = (h.size - 1).toBigInteger()
    while ((i.compareTo(0.toBigInteger()) > 0) && ((h[(i.subtract(1.toBigInteger())).toInt()])["s"] as Int > (h[(i).toInt()])["s"] as Int)) {
        val tmp: MutableMap<String, Int> = h[(i.subtract(1.toBigInteger())).toInt()]
        h[(i.subtract(1.toBigInteger())).toInt()] = h[(i).toInt()]
        h[(i).toInt()] = tmp
        i = i.subtract(1.toBigInteger())
    }
    return h
}

fun step(h: MutableList<MutableMap<String, Int>>, nv: Int, dir: MutableList<Int>): MutableMap<String, Any?> {
    var nv: Int = nv
    var h: MutableList<MutableMap<String, Int>> = h
    while ((h.size == 0) || ((nv * nv) <= (h[0])["s"] as Int)) {
        h = push(h, mutableMapOf<String, Int>("s" to (nv * nv), "a" to (nv), "b" to (0)))
        nv = nv + 1
    }
    val s: Int = (h[0])["s"] as Int
    var v: MutableList<MutableList<Int>> = mutableListOf()
    while ((h.size > 0) && ((h[0])["s"] as Int == s)) {
        val it: MutableMap<String, Int> = h[0]
        h = h.subList(1, h.size)
        v = run { val _tmp = v.toMutableList(); _tmp.add(mutableListOf((it)["a"] as Int, (it)["b"] as Int)); _tmp } as MutableList<MutableList<Int>>
        if ((it)["a"] as Int > (it)["b"] as Int) {
            h = push(h, mutableMapOf<String, Int>("s" to (((it)["a"] as Int * (it)["a"] as Int) + (((it)["b"] as Int + 1) * ((it)["b"] as Int + 1))), "a" to ((it)["a"] as Int), "b" to ((it)["b"] as Int + 1)))
        }
    }
    var list: MutableList<MutableList<Int>> = mutableListOf()
    for (p in v) {
        list = run { val _tmp = list.toMutableList(); _tmp.add(p); _tmp } as MutableList<MutableList<Int>>
    }
    var temp: MutableList<MutableList<Int>> = list
    for (p in temp) {
        if (p[0] != p[1]) {
            list = run { val _tmp = list.toMutableList(); _tmp.add(mutableListOf(p[1], p[0])); _tmp } as MutableList<MutableList<Int>>
        }
    }
    temp = list
    for (p in temp) {
        if (p[1] != 0) {
            list = run { val _tmp = list.toMutableList(); _tmp.add(mutableListOf(p[0], 0 - p[1])); _tmp } as MutableList<MutableList<Int>>
        }
    }
    temp = list
    for (p in temp) {
        if (p[0] != 0) {
            list = run { val _tmp = list.toMutableList(); _tmp.add(mutableListOf(0 - p[0], p[1])); _tmp } as MutableList<MutableList<Int>>
        }
    }
    var bestDot: Int = 0 - 999999999
    var best: MutableList<Int> = dir
    for (p in list) {
        val cross: Int = (p[0] * dir[1]) - (p[1] * dir[0])
        if (cross >= 0) {
            val dot: Int = (p[0] * dir[0]) + (p[1] * dir[1])
            if (dot > bestDot) {
                bestDot = dot
                best = p
            }
        }
    }
    return mutableMapOf<String, Any?>("d" to (best), "heap" to (h), "n" to (nv))
}

fun positions(n: Int): MutableList<MutableList<Int>> {
    var pos: MutableList<MutableList<Int>> = mutableListOf()
    var x: Int = 0
    var y: Int = 0
    var dir: MutableList<Int> = mutableListOf(0, 1)
    var heap: MutableList<MutableMap<String, Int>> = mutableListOf()
    var nv: Int = 1
    var i: Int = 0
    while (i < n) {
        pos = run { val _tmp = pos.toMutableList(); _tmp.add(mutableListOf(x, y)); _tmp } as MutableList<MutableList<Int>>
        val st: MutableMap<String, Any?> = step(heap, nv, dir)
        dir = ((st)["d"] as Any?) as MutableList<Int>
        heap = ((st)["heap"] as Any?) as MutableList<MutableMap<String, Int>>
        nv = (st)["n"] as Int
        x = x + dir[0]
        y = y + dir[1]
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
    val pts: MutableList<MutableList<Int>> = positions(40)
    println("The first 40 Babylonian spiral points are:")
    var line: String = ""
    var i: Int = 0
    while (i < pts.size) {
        val p: MutableList<Int> = pts[i]
        val s: String = pad(((("(" + (p[0]).toString()) + ", ") + (p[1]).toString()) + ")", 10)
        line = line + s
        if ((Math.floorMod((i + 1), 10)) == 0) {
            println(line)
            line = ""
        }
        i = i + 1
    }
}

fun main() {
    user_main()
}
