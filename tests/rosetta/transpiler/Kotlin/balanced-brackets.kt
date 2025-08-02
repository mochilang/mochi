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

var seed: Int = 1
fun prng(max: Int): Int {
    seed = Math.floorMod((((seed * 1103515245) + 12345).toLong()), 2147483648L).toInt()
    return Math.floorMod(seed, max)
}

fun gen(n: Int): String {
    var arr: MutableList<String> = mutableListOf<String>()
    var i: Int = 0
    while (i < n) {
        arr = run { val _tmp = arr.toMutableList(); _tmp.add("["); _tmp } as MutableList<String>
        arr = run { val _tmp = arr.toMutableList(); _tmp.add("]"); _tmp } as MutableList<String>
        i = i + 1
    }
    var j: BigInteger = (arr.size - 1).toBigInteger()
    while (j.compareTo((0).toBigInteger()) > 0) {
        var k: Int = prng((j.add((1).toBigInteger())).toInt())
        var tmp: String = arr[(j).toInt()]!!
        arr[(j).toInt()] = arr[k]!!
        arr[k] = tmp
        j = j.subtract((1).toBigInteger())
    }
    var out: String = ""
    for (ch in arr) {
        out = out + ch
    }
    return out
}

fun testBalanced(s: String): Unit {
    var open: Int = 0
    var i: Int = 0
    while (i < s.length) {
        var c: String = s.substring(i, i + 1)
        if (c == "[") {
            open = open + 1
        } else {
            if (c == "]") {
                if (open == 0) {
                    println(s + ": not ok")
                    return
                }
                open = open - 1
            } else {
                println(s + ": not ok")
                return
            }
        }
        i = i + 1
    }
    if (open == 0) {
        println(s + ": ok")
    } else {
        println(s + ": not ok")
    }
}

fun user_main(): Unit {
    var i: Int = 0
    while (i < 10) {
        testBalanced(gen(i))
        i = i + 1
    }
    testBalanced("()")
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
