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

fun powInt(base: Int, exp: Int): Int {
    var r: Int = 1
    var b: Int = base
    var e: Int = exp
    while (e > 0) {
        if ((Math.floorMod(e, 2)) == 1) {
            r = r * b
        }
        b = b * b
        e = e / 2.toInt()
    }
    return r
}

fun minInt(x: Int, y: Int): Int {
    if (x < y) {
        return x
    }
    return y
}

fun throwDie(nSides: Int, nDice: Int, s: Int, counts: MutableList<Int>): Unit {
    if (nDice == 0) {
        counts[s] = counts[s] + 1
        return
    }
    var i: Int = 1
    while (i <= nSides) {
        throwDie(nSides, nDice - 1, s + i, counts)
        i = i + 1
    }
}

fun beatingProbability(nSides1: Int, nDice1: Int, nSides2: Int, nDice2: Int): Double {
    val len1: Int = (nSides1 + 1) * nDice1
    var c1: MutableList<Int> = mutableListOf<Int>()
    var i: Int = 0
    while (i < len1) {
        c1 = run { val _tmp = c1.toMutableList(); _tmp.add(0); _tmp } as MutableList<Int>
        i = i + 1
    }
    throwDie(nSides1, nDice1, 0, c1)
    val len2: Int = (nSides2 + 1) * nDice2
    var c2: MutableList<Int> = mutableListOf<Int>()
    var j: Int = 0
    while (j < len2) {
        c2 = run { val _tmp = c2.toMutableList(); _tmp.add(0); _tmp } as MutableList<Int>
        j = j + 1
    }
    throwDie(nSides2, nDice2, 0, c2)
    val p12: Double = (powInt(nSides1, nDice1)).toDouble() * (powInt(nSides2, nDice2)).toDouble()
    var tot: Double = 0.0
    i = 0
    while (i < len1) {
        j = 0
        val m: Int = minInt(i, len2)
        while (j < m) {
            tot = tot + ((c1[i] * (c2[j]).toDouble()) / p12)
            j = j + 1
        }
        i = i + 1
    }
    return tot
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println(beatingProbability(4, 9, 6, 6).toString())
        println(beatingProbability(10, 5, 7, 6).toString())
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
