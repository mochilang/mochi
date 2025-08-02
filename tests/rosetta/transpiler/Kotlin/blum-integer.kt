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

fun isPrime(n: Int): Boolean {
    if (n < 2) {
        return false
    }
    if ((Math.floorMod(n, 2)) == 0) {
        return n == 2
    }
    if ((Math.floorMod(n, 3)) == 0) {
        return n == 3
    }
    var d: Int = 5
    while ((d * d) <= n) {
        if ((Math.floorMod(n, d)) == 0) {
            return false
        }
        d = d + 2
        if ((Math.floorMod(n, d)) == 0) {
            return false
        }
        d = d + 4
    }
    return true
}

fun firstPrimeFactor(n: Int): Int {
    if (n == 1) {
        return 1
    }
    if ((Math.floorMod(n, 3)) == 0) {
        return 3
    }
    if ((Math.floorMod(n, 5)) == 0) {
        return 5
    }
    var inc: MutableList<Int> = mutableListOf(4, 2, 4, 2, 4, 6, 2, 6)
    var k: Int = 7
    var i: Int = 0
    while ((k * k) <= n) {
        if ((Math.floorMod(n, k)) == 0) {
            return k
        }
        k = k + inc[i]!!
        i = Math.floorMod((i + 1), inc.size)
    }
    return n
}

fun indexOf(s: String, ch: String): Int {
    var i: Int = 0
    while (i < s.length) {
        if (s.substring(i, i + 1) == ch) {
            return i
        }
        i = i + 1
    }
    return 0 - 1
}

fun padLeft(n: Int, width: Int): String {
    var s: String = n.toString()
    while (s.length < width) {
        s = " " + s
    }
    return s
}

fun formatFloat(f: Double, prec: Int): String {
    var s: String = f.toString()
    var idx: Int = s.indexOf(".")
    if (idx < 0) {
        return s
    }
    var need: BigInteger = ((idx + 1) + prec).toBigInteger()
    if ((s.length).toBigInteger().compareTo((need)) > 0) {
        return s.substring(0, (need).toInt())
    }
    return s
}

fun user_main(): Unit {
    var blum: MutableList<Int> = mutableListOf<Int>()
    var counts: MutableList<Int> = mutableListOf(0, 0, 0, 0)
    var digits: MutableList<Int> = mutableListOf(1, 3, 7, 9)
    var i: Int = 1
    var bc: Int = 0
    while (true) {
        var p: Int = firstPrimeFactor(i)
        if ((Math.floorMod(p, 4)) == 3) {
            var q: Int = ((i / p).toInt())
            if ((((q != p) && ((Math.floorMod(q, 4)) == 3) as Boolean)) && isPrime(q)) {
                if (bc < 50) {
                    blum = run { val _tmp = blum.toMutableList(); _tmp.add(i); _tmp }
                }
                var d: BigInteger = (Math.floorMod(i, 10)).toBigInteger()
                if (d.compareTo((1).toBigInteger()) == 0) {
                    counts[0] = counts[0]!! + 1
                } else {
                    if (d.compareTo((3).toBigInteger()) == 0) {
                        counts[1] = counts[1]!! + 1
                    } else {
                        if (d.compareTo((7).toBigInteger()) == 0) {
                            counts[2] = counts[2]!! + 1
                        } else {
                            if (d.compareTo((9).toBigInteger()) == 0) {
                                counts[3] = counts[3]!! + 1
                            }
                        }
                    }
                }
                bc = bc + 1
                if (bc == 50) {
                    println("First 50 Blum integers:")
                    var idx: Int = 0
                    while (idx < 50) {
                        var line: String = ""
                        var j: Int = 0
                        while (j < 10) {
                            line = (line + padLeft(blum[idx]!!, 3)) + " "
                            idx = idx + 1
                            j = j + 1
                        }
                        println(line.substring(0, line.length - 1))
                    }
                    break
                }
            }
        }
        if ((Math.floorMod(i, 5)) == 3) {
            i = i + 4
        } else {
            i = i + 2
        }
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
