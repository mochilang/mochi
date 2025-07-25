import java.math.BigInteger

var _nowSeed = 0L
var _nowSeeded = false
fun _now(): Int {
    if (!_nowSeeded) {
        System.getenv("MOCHI_NOW_SEED")?.toLongOrNull()?.let {
            _nowSeed = it
            _nowSeeded = true
        }
    }
    return if (_nowSeeded) {
        _nowSeed = (_nowSeed * 1664525 + 1013904223) % 2147483647
        kotlin.math.abs(_nowSeed.toInt())
    } else {
        kotlin.math.abs(System.nanoTime().toInt())
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

var pps: MutableMap<Int, Boolean> = mutableMapOf<Any?, Any?>() as MutableMap<Int, Boolean>
fun pow10(exp: Int): Int {
    var n: Int = 1
    var i: Int = 0
    while (i < exp) {
        n = n * 10
        i = i + 1
    }
    return n
}

fun totient(n: Int): Int {
    var tot: Int = n
    var nn: Int = n
    var i: Int = 2
    while ((i * i) <= nn) {
        if ((nn % i) == 0) {
            while ((nn % i) == 0) {
                nn = nn / i
            }
            tot = tot - (tot / i)
        }
        if (i == 2) {
            i = 1
        }
        i = i + 2
    }
    if (nn > 1) {
        tot = tot - (tot / nn)
    }
    return tot
}

fun getPerfectPowers(maxExp: Int): Unit {
    val upper: Int = pow10(maxExp)
    var i: Int = 2
    while ((i * i) < upper) {
        var p: Int = i
        while (true) {
            p = p * i
            if (p >= upper) {
                break
            }
            (pps)[p] = true
        }
        i = i + 1
    }
}

fun getAchilles(minExp: Int, maxExp: Int): MutableMap<Int, Boolean> {
    val lower: Int = pow10(minExp)
    val upper: Int = pow10(maxExp)
    var achilles: MutableMap<Int, Boolean> = mutableMapOf<Any?, Any?>() as MutableMap<Int, Boolean>
    var b: Int = 1
    while (((b * b) * b) < upper) {
        val b3: BigInteger = (b * b) * b
        var a: Int = 1
        while (true) {
            val p: BigInteger = (b3.multiply(a.toBigInteger())).multiply(a.toBigInteger())
            if (p.compareTo(upper.toBigInteger()) >= 0) {
                break
            }
            if (p.compareTo(lower.toBigInteger()) >= 0) {
                if (!((p /* unsupported */ ) as Boolean)) {
                    (achilles)[(p).toInt()] = true
                }
            }
            a = a + 1
        }
        b = b + 1
    }
    return achilles
}

fun sortInts(xs: MutableList<Int>): MutableList<Int> {
    var res: MutableList<Int> = mutableListOf()
    var tmp: MutableList<Int> = xs
    while (tmp.size > 0) {
        var min: Int = tmp[0]
        var idx: Int = 0
        var i: Int = 1
        while (i < tmp.size) {
            if (tmp[i] < min) {
                min = tmp[i]
                idx = i
            }
            i = i + 1
        }
        res = (res + mutableListOf(min)).toMutableList()
        var out: MutableList<Int> = mutableListOf()
        var j: Int = 0
        while (j < tmp.size) {
            if (j != idx) {
                out = (out + mutableListOf(tmp[j])).toMutableList()
            }
            j = j + 1
        }
        tmp = out
    }
    return res
}

fun pad(n: Int, width: Int): String {
    var s: String = n.toString()
    while (s.length < width) {
        s = " " + s
    }
    return s
}

fun user_main(): Unit {
    val maxDigits: Int = 15
    getPerfectPowers(5)
    val achSet: MutableMap<Int, Boolean> = getAchilles(1, 5)
    var ach: MutableList<Int> = mutableListOf()
    for (k in (achSet)["keys"]!!().keys) {
        ach = ((ach + mutableListOf(k)).toMutableList()) as MutableList<Int>
    }
    ach = sortInts(ach)
    println("First 50 Achilles numbers:")
    var i: Int = 0
    while (i < 50) {
        var line: String = ""
        var j: Int = 0
        while (j < 10) {
            line = line + pad(ach[i], 4)
            if (j < 9) {
                line = line + " "
            }
            i = i + 1
            j = j + 1
        }
        println(line)
    }
    println("\nFirst 30 strong Achilles numbers:")
    var strong: MutableList<Int> = mutableListOf()
    var count: Int = 0
    var idx: Int = 0
    while (count < 30) {
        val tot: Int = totient(ach[idx])
        if (tot in achSet) {
            strong = (strong + mutableListOf(ach[idx])).toMutableList()
            count = count + 1
        }
        idx = idx + 1
    }
    i = 0
    while (i < 30) {
        var line: String = ""
        var j: Int = 0
        while (j < 10) {
            line = line + pad(strong[i], 5)
            if (j < 9) {
                line = line + " "
            }
            i = i + 1
            j = j + 1
        }
        println(line)
    }
    println("\nNumber of Achilles numbers with:")
    val counts: MutableList<Int> = mutableListOf(1, 12, 47, 192, 664, 2242, 7395, 24008, 77330, 247449, 788855, 2508051, 7960336, 25235383)
    var d: Int = 2
    while (d <= maxDigits) {
        val c: Int = counts[d - 2]
        println((pad(d, 2) + " digits: ") + c.toString())
        d = d + 1
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
