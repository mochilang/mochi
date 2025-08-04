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

fun modPow(base: Int, exp: Int, m: Int): Int {
    var result: BigInteger = (Math.floorMod(1, m)).toBigInteger()
    var b: BigInteger = (Math.floorMod(base, m)).toBigInteger()
    var e: Int = exp
    while (e > 0) {
        if ((Math.floorMod(e, 2)) == 1) {
            result = (result.multiply((b))).remainder((m).toBigInteger())
        }
        b = (b.multiply((b))).remainder((m).toBigInteger())
        e = ((e / 2).toInt())
    }
    return (result.toInt())
}

fun isPrime(n: Int): Boolean {
    if (n < 2) {
        return false
    }
    for (p in mutableListOf(2, 3, 5, 7, 11, 13, 17, 19, 23, 29)) {
        if ((Math.floorMod(n, p)) == 0) {
            return n == p
        }
    }
    var d: BigInteger = (n - 1).toBigInteger()
    var s: Int = 0
    while ((d.remainder((2).toBigInteger())).compareTo((0).toBigInteger()) == 0) {
        d = d.divide((2).toBigInteger())
        s = s + 1
    }
    for (a in mutableListOf(2, 325, 9375, 28178, 450775, 9780504, 1795265022)) {
        if ((Math.floorMod(a, n)) == 0) {
            return true
        }
        var x: Int = modPow(a, (d.toInt()), n)
        if ((x == 1) || (x == (n - 1))) {
            continue
        }
        var r: Int = 1
        var passed: Boolean = false
        while (r < s) {
            x = Math.floorMod((x * x), n)
            if (x == (n - 1)) {
                passed = true
                break
            }
            r = r + 1
        }
        if (!passed) {
            return false
        }
    }
    return true
}

fun commatize(n: Int): String {
    var s: String = n.toString()
    var i: BigInteger = (s.length - 3).toBigInteger()
    while (i.compareTo((0).toBigInteger()) > 0) {
        s = (s.substring(0, (i).toInt()) + ",") + s.substring((i).toInt(), s.length)
        i = i.subtract((3).toBigInteger())
    }
    return s
}

fun pad(s: String, width: Int): String {
    var out: String = s
    while (out.length < width) {
        out = " " + out
    }
    return out
}

fun join(xs: MutableList<String>, sep: String): String {
    var res: String = ""
    var i: Int = 0
    while (i < xs.size) {
        if (i > 0) {
            res = res + sep
        }
        res = res + xs[i]!!
        i = i + 1
    }
    return res
}

fun formatRow(row: MutableList<String>): String {
    var padded: MutableList<String> = mutableListOf<String>()
    var i: Int = 0
    while (i < row.size) {
        padded = run { val _tmp = padded.toMutableList(); _tmp.add(pad(row[i]!!, 9)); _tmp }
        i = i + 1
    }
    return ("[" + join(padded, " ")) + "]"
}

fun user_main(): Unit {
    var cubans: MutableList<String> = mutableListOf<String>()
    var cube1: Int = 1
    var count: Int = 0
    var cube100k: Int = 0
    var i: Int = 1
    while (true) {
        var j: BigInteger = (i + 1).toBigInteger()
        var cube2: BigInteger = (j.multiply((j))).multiply((j))
        var diff: BigInteger = cube2.subtract((cube1).toBigInteger())
        if (((isPrime((diff.toInt()))) as Boolean)) {
            if (count < 200) {
                cubans = run { val _tmp = cubans.toMutableList(); _tmp.add(commatize((diff.toInt()))); _tmp }
            }
            count = count + 1
            if (count == 100000) {
                cube100k = (diff.toInt())
                break
            }
        }
        cube1 = (cube2.toInt())
        i = i + 1
    }
    println("The first 200 cuban primes are:-")
    var row: Int = 0
    while (row < 20) {
        var slice: MutableList<String> = mutableListOf<String>()
        var k: Int = 0
        while (k < 10) {
            slice = run { val _tmp = slice.toMutableList(); _tmp.add(cubans[(row * 10) + k]!!); _tmp }
            k = k + 1
        }
        println(formatRow(slice))
        row = row + 1
    }
    println("\nThe 100,000th cuban prime is " + commatize(cube100k))
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
