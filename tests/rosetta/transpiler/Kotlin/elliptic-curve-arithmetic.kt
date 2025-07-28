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

data class Pt(var x: Double, var y: Double, var inf: Boolean)
val bCoeff: Double = 7.0
fun zero(): Pt {
    return Pt(x = 0.0, y = 0.0, inf = true)
}

fun isZero(p: Pt): Boolean {
    return p.inf
}

fun neg(p: Pt): Pt {
    return Pt(x = p.x, y = 0.0 - p.y, inf = p.inf)
}

fun dbl(p: Pt): Pt {
    if ((isZero(p)) as Boolean) {
        return p
    }
    val L: Double = ((3.0 * p.x) * p.x) / (2.0 * p.y)
    val x: Double = (L * L) - (2.0 * p.x)
    return Pt(x = x, y = (L * (p.x - x)) - p.y, inf = false)
}

fun add(p: Pt, q: Pt): Pt {
    if ((isZero(p)) as Boolean) {
        return q
    }
    if ((isZero(q)) as Boolean) {
        return p
    }
    if (p.x == q.x) {
        if (p.y == q.y) {
            return dbl(p)
        }
        return zero()
    }
    val L: Double = (q.y - p.y) / (q.x - p.x)
    val x: Double = ((L * L) - p.x) - q.x
    return Pt(x = x, y = (L * (p.x - x)) - p.y, inf = false)
}

fun mul(p: Pt, n: Int): Pt {
    var r: Pt = zero()
    var q: Pt = p
    var k: Int = n
    while (k > 0) {
        if ((Math.floorMod(k, 2)) == 1) {
            r = add(r, q)
        }
        q = dbl(q)
        k = k / 2
    }
    return r
}

fun cbrtApprox(x: Double): Double {
    var guess: Double = x
    var i: Int = 0
    while (i < 40) {
        guess = ((2.0 * guess) + (x / (guess * guess))) / 3.0
        i = i + 1
    }
    return guess
}

fun fromY(y: Double): Pt {
    return Pt(x = cbrtApprox((y * y) - bCoeff), y = y, inf = false)
}

fun show(s: String, p: Pt): Unit {
    if ((isZero(p)) as Boolean) {
        println(s + "Zero")
    } else {
        println(((((s + "(") + p.x.toString()) + ", ") + p.y.toString()) + ")")
    }
}

fun user_main(): Unit {
    val a: Pt = fromY(1.0)
    val b: Pt = fromY(2.0)
    show("a = ", a)
    show("b = ", b)
    val c: Pt = add(a, b)
    show("c = a + b = ", c)
    val d: Pt = neg(c)
    show("d = -c = ", d)
    show("c + d = ", add(c, d))
    show("a + b + d = ", add(a, add(b, d)))
    show("a * 12345 = ", mul(a, 12345))
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
