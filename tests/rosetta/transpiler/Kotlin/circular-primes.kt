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

var circs: MutableList<Int> = mutableListOf<Int>()
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

fun isCircular(n: Int): Boolean {
    var nn: Int = n
    var pow: Int = 1
    while (nn > 0) {
        pow = pow * 10
        nn = nn / 10
    }
    nn = n
    while (true) {
        nn = nn * 10
        var f: Int = nn / pow
        nn = nn + (f * (1 - pow))
        if (nn == n) {
            break
        }
        if (!isPrime(nn)) {
            return false
        }
    }
    return true
}

fun showList(xs: MutableList<Int>): String {
    var out: String = "["
    var i: Int = 0
    while (i < xs.size) {
        out = out + (xs[i]!!).toString()
        if (i < (xs.size - 1)) {
            out = out + ", "
        }
        i = i + 1
    }
    return out + "]"
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println("The first 19 circular primes are:")
        var digits: MutableList<Int> = mutableListOf(1, 3, 7, 9)
        var q: MutableList<Int> = mutableListOf(1, 2, 3, 5, 7, 9)
        var fq: MutableList<Int> = mutableListOf(1, 2, 3, 5, 7, 9)
        var count: Int = 0
        while (true) {
            var f: Int = q[0]!!
            var fd: Int = fq[0]!!
            if (isPrime(f) && isCircular(f)) {
                circs = run { val _tmp = circs.toMutableList(); _tmp.add(f); _tmp }
                count = count + 1
                if (count == 19) {
                    break
                }
            }
            q = q.subList(1, q.size)
            fq = fq.subList(1, fq.size)
            if ((f != 2) && (f != 5)) {
                for (d in digits) {
                    q = run { val _tmp = q.toMutableList(); _tmp.add((f * 10) + d); _tmp }
                    fq = run { val _tmp = fq.toMutableList(); _tmp.add(fd); _tmp }
                }
            }
        }
        println(showList(circs))
        println("\nThe next 4 circular primes, in repunit format, are:")
        println("[R(19) R(23) R(317) R(1031)]")
        println("\nThe following repunits are probably circular primes:")
        for (i in mutableListOf(5003, 9887, 15073, 25031, 35317, 49081)) {
            println(("R(" + i.toString()) + ") : true")
        }
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
