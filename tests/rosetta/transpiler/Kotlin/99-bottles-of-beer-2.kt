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

fun fields(s: String): MutableList<String> {
    var words: MutableList<String> = mutableListOf()
    var cur: String = ""
    var i: Int = 0
    while (i < s.length) {
        val ch: String = s.substring(i, i + 1)
        if ((((ch == " ") || (ch == "\n") as Boolean)) || (ch == "\t")) {
            if (cur.length > 0) {
                words = run { val _tmp = words.toMutableList(); _tmp.add(cur); _tmp } as MutableList<String>
                cur = ""
            }
        } else {
            cur = cur + ch
        }
        i = i + 1
    }
    if (cur.length > 0) {
        words = run { val _tmp = words.toMutableList(); _tmp.add(cur); _tmp } as MutableList<String>
    }
    return words
}

fun join(xs: MutableList<String>, sep: String): String {
    var res: String = ""
    var i: Int = 0
    while (i < xs.size) {
        if (i > 0) {
            res = res + sep
        }
        res = res + xs[i]
        i = i + 1
    }
    return res
}

fun numberName(n: Int): String {
    val small: MutableList<String> = mutableListOf("no", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen", "sixteen", "seventeen", "eighteen", "nineteen")
    val tens: MutableList<String> = mutableListOf("ones", "ten", "twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty", "ninety")
    if (n < 0) {
        return ""
    }
    if (n < 20) {
        return small[n]
    }
    if (n < 100) {
        var t: String = tens[(n / 10).toInt()]
        var s: BigInteger = n % 10
        if (s.compareTo(0.toBigInteger()) > 0) {
            t = (t + " ") + (small)[(s).toInt()] as String
        }
        return t
    }
    return ""
}

fun pluralizeFirst(s: String, n: Int): String {
    if (n == 1) {
        return s
    }
    val w: MutableList<String> = fields(s)
    if (w.size > 0) {
        w[0] = w[0] + "s"
    }
    return join(w, " ")
}

fun randInt(seed: Int, n: Int): Int {
    val next: BigInteger = ((seed * 1664525) + 1013904223) % 2147483647
    return (next.remainder(n.toBigInteger())) as Int
}

fun slur(p: String, d: Int): String {
    if (p.length <= 2) {
        return p
    }
    var a: MutableList<String> = mutableListOf()
    var i: Int = 1
    while (i < (p.length - 1)) {
        a = run { val _tmp = a.toMutableList(); _tmp.add(p.substring(i, i + 1)); _tmp } as MutableList<String>
        i = i + 1
    }
    var idx: BigInteger = a.size - 1
    var seed: Int = d
    while (idx.compareTo(1.toBigInteger()) >= 0) {
        seed = ((seed * 1664525) + 1013904223) % 2147483647
        if ((seed % 100) >= d) {
            val j: BigInteger = seed.toBigInteger().remainder((idx.add(1.toBigInteger())))
            val tmp: String = (a)[(idx).toInt()] as String
            a[(idx).toInt()] = (a)[(j).toInt()] as String
            a[(j).toInt()] = tmp
        }
        idx = idx.subtract(1.toBigInteger())
    }
    var s: String = p.substring(0, 1)
    var k: Int = 0
    while (k < a.size) {
        s = s + a[k]
        k = k + 1
    }
    s = s + (p.substring(p.length - 1, p.length)).toString()
    val w: MutableList<String> = fields(s)
    return join(w, " ")
}

fun user_main(): Unit {
    var i: Int = 99
    while (i > 0) {
        println((((slur(numberName(i), i) + " ") + pluralizeFirst(slur("bottle of", i), i)) + " ") + slur("beer on the wall", i))
        println((((slur(numberName(i), i) + " ") + pluralizeFirst(slur("bottle of", i), i)) + " ") + slur("beer", i))
        println((((slur("take one", i) + " ") + slur("down", i)) + " ") + slur("pass it around", i))
        println((((slur(numberName(i - 1), i) + " ") + pluralizeFirst(slur("bottle of", i), i - 1)) + " ") + slur("beer on the wall", i))
        i = i - 1
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
