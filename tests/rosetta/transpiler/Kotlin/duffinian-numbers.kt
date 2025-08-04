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

fun gcd(a: Int, b: Int): Int {
    var x: Int = a
    if (x < 0) {
        x = 0 - x
    }
    var y: Int = b
    if (y < 0) {
        y = 0 - y
    }
    while (y != 0) {
        var t: BigInteger = (Math.floorMod(x, y)).toBigInteger()
        x = y
        y = (t.toInt())
    }
    return x
}

fun divisors(n: Int): MutableList<Int> {
    var divs: MutableList<Int> = mutableListOf<Int>()
    var i: Int = 1
    while ((i * i) <= n) {
        if ((Math.floorMod(n, i)) == 0) {
            divs = run { val _tmp = divs.toMutableList(); _tmp.add(i); _tmp }
            var j: Int = ((n / i).toInt())
            if (i != j) {
                divs = run { val _tmp = divs.toMutableList(); _tmp.add(j); _tmp }
            }
        }
        i = i + 1
    }
    return divs
}

fun sum(xs: MutableList<Int>): Int {
    var s: Int = 0
    for (v in xs) {
        s = s + v
    }
    return s
}

fun isDuffinian(n: Int): Boolean {
    var divs: MutableList<Int> = divisors(n)
    if (divs.size <= 2) {
        return false
    }
    var sigma: Int = (divs.sum() as Int)
    return gcd(sigma, n) == 1
}

fun pad(n: Int, width: Int): String {
    var s: String = n.toString()
    while (s.length < width) {
        s = " " + s
    }
    return s
}

fun printTable(nums: MutableList<Int>, perRow: Int, width: Int): Unit {
    var i: Int = 0
    var line: String = ""
    while (i < nums.size) {
        line = (line + " ") + pad(nums[i]!!, width)
        if ((Math.floorMod((i + 1), perRow)) == 0) {
            println(line.substring(1, line.length))
            line = ""
        }
        i = i + 1
    }
    if (line.length > 0) {
        println(line.substring(1, line.length))
    }
}

fun user_main(): Unit {
    var duff: MutableList<Int> = mutableListOf<Int>()
    var n: Int = 1
    while (duff.size < 50) {
        if (((isDuffinian(n)) as Boolean)) {
            duff = run { val _tmp = duff.toMutableList(); _tmp.add(n); _tmp }
        }
        n = n + 1
    }
    println("First 50 Duffinian numbers:")
    printTable(duff, 10, 3)
    var triplets: MutableList<String> = mutableListOf<String>()
    n = 1
    while (triplets.size < 20) {
        if (((isDuffinian(n) && isDuffinian(n + 1) as Boolean)) && isDuffinian(n + 2)) {
            triplets = run { val _tmp = triplets.toMutableList(); _tmp.add(((((("(" + n.toString()) + ",") + (n + 1).toString()) + ",") + (n + 2).toString()) + ")"); _tmp }
            n = n + 3
        }
        n = n + 1
    }
    println("\nFirst 20 Duffinian triplets:")
    var i: Int = 0
    while (i < triplets.size) {
        var line: String = ""
        var j: Int = 0
        while ((j < 4) && (i < triplets.size)) {
            line = line + padStr(triplets[i]!!, 16)
            j = j + 1
            i = i + 1
        }
        println(line)
    }
}

fun padStr(s: String, width: Int): String {
    var res: String = s
    while (res.length < width) {
        res = res + " "
    }
    return res
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
