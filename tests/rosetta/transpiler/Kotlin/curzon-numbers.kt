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

fun padLeft(n: Int, width: Int): String {
    var s: String = n.toString()
    while (s.length < width) {
        s = " " + s
    }
    return s
}

fun modPow(base: Int, exp: Int, mod: Int): Int {
    var result: BigInteger = (Math.floorMod(1, mod)).toBigInteger()
    var b: BigInteger = (Math.floorMod(base, mod)).toBigInteger()
    var e: Int = exp
    while (e > 0) {
        if ((Math.floorMod(e, 2)) == 1) {
            result = (result.multiply((b))).remainder((mod).toBigInteger())
        }
        b = (b.multiply((b))).remainder((mod).toBigInteger())
        e = e / 2
    }
    return (result.toInt())
}

fun user_main(): Unit {
    var k: Int = 2
    while (k <= 10) {
        println(("The first 50 Curzon numbers using a base of " + k.toString()) + " :")
        var count: Int = 0
        var n: Int = 1
        var curzon50: MutableList<Int> = mutableListOf<Int>()
        while (true) {
            var d: BigInteger = ((k * n) + 1).toBigInteger()
            if ((((modPow(k, n, (d.toInt())) + 1)).toBigInteger().remainder((d))).compareTo((0).toBigInteger()) == 0) {
                if (count < 50) {
                    curzon50 = run { val _tmp = curzon50.toMutableList(); _tmp.add(n); _tmp }
                }
                count = count + 1
                if (count == 50) {
                    var idx: Int = 0
                    while (idx < curzon50.size) {
                        var line: String = ""
                        var j: Int = 0
                        while (j < 10) {
                            line = (line + padLeft(curzon50[idx]!!, 4)) + " "
                            idx = idx + 1
                            j = j + 1
                        }
                        println(line.substring(0, line.length - 1))
                    }
                }
                if (count == 1000) {
                    println("\nOne thousandth: " + n.toString())
                    break
                }
            }
            n = n + 1
        }
        println("")
        k = k + 2
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
