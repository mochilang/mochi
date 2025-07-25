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

fun divisors(n: Int): MutableList<Int> {
    var divs: MutableList<Int> = mutableListOf(1)
    var divs2: MutableList<Int> = mutableListOf()
    var i: Int = 2
    while ((i * i) <= n) {
        if ((n % i) == 0) {
            val j: Int = (n / i).toInt()
            divs = run { val _tmp = divs.toMutableList(); _tmp.add(i); _tmp } as MutableList<Int>
            if (i != j) {
                divs2 = run { val _tmp = divs2.toMutableList(); _tmp.add(j); _tmp } as MutableList<Int>
            }
        }
        i = i + 1
    }
    var j: Int = divs2.size - 1
    while (j >= 0) {
        divs = run { val _tmp = divs.toMutableList(); _tmp.add(divs2[j]); _tmp } as MutableList<Int>
        j = j - 1
    }
    return divs
}

fun sum(xs: MutableList<Int>): Int {
    var tot: Int = 0
    for (v in xs) {
        tot = tot + v
    }
    return tot
}

fun sumStr(xs: MutableList<Int>): String {
    var s: String = ""
    var i: Int = 0
    while (i < xs.size) {
        s = (s + (xs[i]).toString()) + " + "
        i = i + 1
    }
    return s.substring(0, s.length - 3) as String
}

fun pad2(n: Int): String {
    val s: String = n.toString()
    if (s.length < 2) {
        return " " + s
    }
    return s
}

fun pad5(n: Int): String {
    var s: String = n.toString()
    while (s.length < 5) {
        s = " " + s
    }
    return s
}

fun abundantOdd(searchFrom: Int, countFrom: Int, countTo: Int, printOne: Boolean): Int {
    var count: Int = countFrom
    var n: Int = searchFrom
    while (count < countTo) {
        val divs: MutableList<Int> = divisors(n)
        val tot: Int = divs.sum()
        if (tot > n) {
            count = count + 1
            if (printOne && (count < countTo)) {
                n = n + 2
                continue
            }
            val s: String = sumStr(divs)
            if (!printOne) {
                println((((((pad2(count) + ". ") + pad5(n)) + " < ") + s) + " = ") + tot.toString())
            } else {
                println((((n.toString() + " < ") + s) + " = ") + tot.toString())
            }
        }
        n = n + 2
    }
    return n
}

fun user_main(): Unit {
    val max: Int = 25
    println(("The first " + max.toString()) + " abundant odd numbers are:")
    val n: Int = abundantOdd(1, 0, max, false)
    println("\nThe one thousandth abundant odd number is:")
    abundantOdd(n as Int, max, 1000, true)
    println("\nThe first abundant odd number above one billion is:")
    abundantOdd(1000000001, 0, 1, true)
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
