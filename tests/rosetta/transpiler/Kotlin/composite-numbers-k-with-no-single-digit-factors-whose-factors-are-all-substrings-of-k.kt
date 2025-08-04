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

fun primeFactors(n: Int): MutableList<Int> {
    var factors: MutableList<Int> = mutableListOf<Int>()
    var x: Int = n
    while ((Math.floorMod(x, 2)) == 0) {
        factors = run { val _tmp = factors.toMutableList(); _tmp.add(2); _tmp }
        x = ((x / 2).toInt())
    }
    var p: Int = 3
    while ((p * p) <= x) {
        while ((Math.floorMod(x, p)) == 0) {
            factors = run { val _tmp = factors.toMutableList(); _tmp.add(p); _tmp }
            x = ((x / p).toInt())
        }
        p = p + 2
    }
    if (x > 1) {
        factors = run { val _tmp = factors.toMutableList(); _tmp.add(x); _tmp }
    }
    return factors
}

fun commatize(n: Int): String {
    var s: String = n.toString()
    var out: String = ""
    var i: BigInteger = (s.length - 1).toBigInteger()
    var c: Int = 0
    while (i.compareTo((0).toBigInteger()) >= 0) {
        out = s.substring((i).toInt(), (i.add((1).toBigInteger())).toInt()) + out
        c = c + 1
        if (((Math.floorMod(c, 3)) == 0) && (i.compareTo((0).toBigInteger()) > 0)) {
            out = "," + out
        }
        i = i.subtract((1).toBigInteger())
    }
    return out
}

fun indexOf(s: String, sub: String): Int {
    var i: Int = 0
    while ((i + sub.length) <= s.length) {
        if (s.substring(i, i + sub.length) == sub) {
            return i
        }
        i = i + 1
    }
    return 0 - 1
}

fun pad10(s: String): String {
    var str: String = s
    while (str.length < 10) {
        str = " " + str
    }
    return str
}

fun trimRightStr(s: String): String {
    var end: Int = s.length
    while ((end > 0) && (s.substring(end - 1, end) == " ")) {
        end = end - 1
    }
    return s.substring(0, end)
}

fun user_main(): Unit {
    var res: MutableList<Int> = mutableListOf<Int>()
    var count: Int = 0
    var k: BigInteger = (11 * 11).toBigInteger()
    while (count < 20) {
        if (((((k.remainder((3).toBigInteger())).compareTo((0).toBigInteger()) == 0) || ((k.remainder((5).toBigInteger())).compareTo((0).toBigInteger()) == 0) as Boolean)) || ((k.remainder((7).toBigInteger())).compareTo((0).toBigInteger()) == 0)) {
            k = k.add((2).toBigInteger())
            continue
        }
        var factors: MutableList<Int> = primeFactors((k.toInt()))
        if (factors.size > 1) {
            var s: String = k.toString()
            var includesAll: Boolean = true
            var prev: Int = 0 - 1
            for (f in factors) {
                if (f == prev) {
                    continue
                }
                var fs: String = f.toString()
                if (s.indexOf(fs) == (0 - 1)) {
                    includesAll = false
                    break
                }
                prev = f
            }
            if ((includesAll as Boolean)) {
                res = run { val _tmp = res.toMutableList(); _tmp.add((k.toInt())); _tmp }
                count = count + 1
            }
        }
        k = k.add((2).toBigInteger())
    }
    var line: String = ""
    for (e in res.subList(0, 10)) {
        line = (line + pad10(commatize(e))) + " "
    }
    println(trimRightStr(line))
    line = ""
    for (e in res.subList(10, 20)) {
        line = (line + pad10(commatize(e))) + " "
    }
    println(trimRightStr(line))
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
