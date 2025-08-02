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

fun trimLeftZeros(s: String): String {
    var i: Int = 0
    while ((i < s.length) && (s.substring(i, i + 1) == "0")) {
        i = i + 1
    }
    return s.substring(i, s.length)
}

fun btString(s: String): MutableMap<String, Any?> {
    var s: String = s
    s = trimLeftZeros(s)
    var b: MutableList<Int> = mutableListOf<Int>()
    var i: BigInteger = (s.length - 1).toBigInteger()
    while (i.compareTo((0).toBigInteger()) >= 0) {
        var ch: String = s.substring((i).toInt(), (i.add((1).toBigInteger())).toInt())
        if (ch == "+") {
            b = run { val _tmp = b.toMutableList(); _tmp.add(1); _tmp } as MutableList<Int>
        } else {
            if (ch == "0") {
                b = run { val _tmp = b.toMutableList(); _tmp.add(0); _tmp } as MutableList<Int>
            } else {
                if (ch == "-") {
                    b = run { val _tmp = b.toMutableList(); _tmp.add(0 - 1); _tmp } as MutableList<Int>
                } else {
                    return mutableMapOf<String, Any?>("bt" to (mutableListOf<Any?>()), "ok" to (false))
                }
            }
        }
        i = i.subtract((1).toBigInteger())
    }
    return mutableMapOf<String, Any?>("bt" to (b), "ok" to (true))
}

fun btToString(b: MutableList<Int>): String {
    if (b.size == 0) {
        return "0"
    }
    var r: String = ""
    var i: BigInteger = (b.size - 1).toBigInteger()
    while (i.compareTo((0).toBigInteger()) >= 0) {
        var d: Int = b[(i).toInt()]!!
        if (d == (0 - 1)) {
            r = r + "-"
        } else {
            if (d == 0) {
                r = r + "0"
            } else {
                r = r + "+"
            }
        }
        i = i.subtract((1).toBigInteger())
    }
    return r
}

fun btInt(i: Int): MutableList<Int> {
    if (i == 0) {
        return mutableListOf<Int>()
    }
    var n: Int = i
    var b: MutableList<Int> = mutableListOf<Int>()
    while (n != 0) {
        var m: BigInteger = (Math.floorMod(n, 3)).toBigInteger()
        n = (n / 3).toInt()
        if (m.compareTo((2).toBigInteger()) == 0) {
            m = (0 - 1).toBigInteger()
            n = n + 1
        } else {
            if (m.compareTo((0 - 2).toBigInteger()) == 0) {
                m = 1.toBigInteger()
                n = n - 1
            }
        }
        b = run { val _tmp = b.toMutableList(); _tmp.add(m.toInt()); _tmp } as MutableList<Int>
    }
    return b
}

fun btToInt(b: MutableList<Int>): Int {
    var r: Int = 0
    var pt: Int = 1
    var i: Int = 0
    while (i < b.size) {
        r = r + (b[i]!! * pt)
        pt = pt * 3
        i = i + 1
    }
    return r
}

fun btNeg(b: MutableList<Int>): MutableList<Int> {
    var r: MutableList<Int> = mutableListOf<Int>()
    var i: Int = 0
    while (i < b.size) {
        r = run { val _tmp = r.toMutableList(); _tmp.add(0 - b[i]!!); _tmp } as MutableList<Int>
        i = i + 1
    }
    return r
}

fun btAdd(a: MutableList<Int>, b: MutableList<Int>): MutableList<Int> {
    return btInt(btToInt(a) + btToInt(b))
}

fun btMul(a: MutableList<Int>, b: MutableList<Int>): MutableList<Int> {
    return btInt(btToInt(a) * btToInt(b))
}

fun padLeft(s: String, w: Int): String {
    var r: String = s
    while (r.length < w) {
        r = " " + r
    }
    return r
}

fun show(label: String, b: MutableList<Int>): Unit {
    var l: String = padLeft(label, 7)
    var bs: String = padLeft(btToString(b), 12)
    var _is: String = padLeft(btToInt(b).toString(), 7)
    println((((l + " ") + bs) + " ") + _is)
}

fun user_main(): Unit {
    var ares: MutableMap<String, Any?> = btString("+-0++0+")
    var a: Any? = (ares)["bt"] as Any?
    var b: MutableList<Int> = btInt(0 - 436)
    var cres: MutableMap<String, Any?> = btString("+-++-")
    var c: Any? = (cres)["bt"] as Any?
    show("a:", a as MutableList<Int>)
    show("b:", b)
    show("c:", c as MutableList<Int>)
    show("a(b-c):", btMul(a as MutableList<Int>, btAdd(b, btNeg(c as MutableList<Int>))))
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
