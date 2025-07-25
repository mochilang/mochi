// Generated by Mochi compiler v0.10.28 on 2006-01-02T15:04:05Z
fun <T> append(list: MutableList<T>, item: T): MutableList<T> {
    val res = list.toMutableList()
    res.add(item)
    return res
}
// Code generated from babylonian-spiral.mochi

/**
 * Auto-generated from Mochi
 * @param h MutableList<MutableMap<String, Int>>
 * @param it MutableMap<String, Int>
 * @return MutableList<MutableMap<String, Int>>
 */
fun push(h: MutableList<MutableMap<String, Int>>, it: MutableMap<String, Int>): MutableList<MutableMap<String, Int>> {
    h = append(h, it)
    var i = h.size - 1
    while (i > 0 && (h[i - 1] as MutableMap<String, Int>)["s"] > (h[i] as MutableMap<String, Int>)["s"]) {
        val tmp = h[i - 1]
        h[i - 1] = h[i]
        h[i] = tmp
        i = i - 1
    }
    return h
}

/**
 * Auto-generated from Mochi
 * @param h MutableList<MutableMap<String, Int>>
 * @param nv Int
 * @param dir MutableList<Int>
 * @return MutableMap<String, Any>
 */
fun step(h: MutableList<MutableMap<String, Int>>, nv: Int, dir: MutableList<Int>): MutableMap<String, Any> {
    while (h.size == 0 || nv * nv <= (h[0] as MutableMap<String, Int>)["s"]) {
        h = push(h, mutableMapOf("s" to (nv * nv), "a" to nv, "b" to 0))
        nv = nv + 1
    }
    val s = (h[0] as MutableMap<String, Int>)["s"]
    var v: MutableList<MutableList<Int>> = mutableListOf<MutableList<Int>>()
    while (h.size > 0 && (h[0] as MutableMap<String, Int>)["s"] == s) {
        val it = h[0]
        h = h.subList(1, h.size)
        v = append(v, mutableListOf(it["a"], it["b"]))
        if (it["a"] > it["b"]) {
            h = push(h, mutableMapOf("s" to (it["a"] * it["a"] + (it["b"] + 1) * (it["b"] + 1)), "a" to (it["a"]), "b" to (it["b"] + 1)))
        }
    }
    var list: MutableList<MutableList<Int>> = mutableListOf<MutableList<Int>>()
    for (p in v) {
        list = append(list, p)
    }
    var temp: MutableList<MutableList<Int>> = list
    for (p in temp) {
        if (p[0] != p[1]) {
            list = append(list, mutableListOf(p[1], p[0]))
        }
    }
    temp = list
    for (p in temp) {
        if (p[1] != 0) {
            list = append(list, mutableListOf(p[0], -p[1]))
        }
    }
    temp = list
    for (p in temp) {
        if (p[0] != 0) {
            list = append(list, mutableListOf(-p[0], p[1]))
        }
    }
    var bestDot = -999999999
    var best = dir
    for (p in list) {
        val cross = p[0] * dir[1] - p[1] * dir[0]
        if (cross >= 0) {
            val dot = p[0] * dir[0] + p[1] * dir[1]
            if (dot > bestDot) {
                bestDot = dot
                best = p
            }
        }
    }
    return mutableMapOf("d" to best, "heap" to h, "n" to nv)
}

/**
 * Auto-generated from Mochi
 * @param n Int
 * @return MutableList<MutableList<Int>>
 */
fun positions(n: Int): MutableList<MutableList<Int>> {
    var pos: MutableList<MutableList<Int>> = mutableListOf<MutableList<Int>>()
    var x = 0
    var y = 0
    var dir: MutableList<Int> = mutableListOf(0, 1)
    var heap: MutableList<MutableMap<String, Int>> = mutableListOf<MutableMap<String, Int>>()
    var nv = 1
    var i = 0
    while (i < n) {
        pos = append(pos, mutableListOf(x, y))
        val st = step(heap, nv, dir)
        dir = (st["d"] as Any?) as MutableList<Int>
        heap = (st["heap"] as Any?) as MutableList<MutableMap<String, Int>>
        nv = ((st["n"] as Any?)).toInt()
        x = x + dir[0]
        y = y + dir[1]
        i = i + 1
    }
    return pos
}

/**
 * Auto-generated from Mochi
 * @param s String
 * @param w Int
 * @return String
 */
fun pad(s: String, w: Int): String {
    var r = s
    while (r.length < w) {
        r = r + " "
    }
    return r
}

/**
 * Auto-generated from Mochi
 */
fun main(): Unit {
    val pts = positions(40)
    println("The first 40 Babylonian spiral points are:")
    var line = ""
    var i = 0
    while (i < pts.size) {
        val p = pts[i]
        val s = pad("(" + p[0].toString() + ", " + p[1].toString() + ")", 10)
        line = line + s
        if ((i + 1) % 10 == 0) {
            println(line)
            line = ""
        }
        i = i + 1
    }
}

