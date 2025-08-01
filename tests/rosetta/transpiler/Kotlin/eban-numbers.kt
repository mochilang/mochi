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

val vals: MutableList<Int> = mutableListOf(0, 2, 4, 6, 30, 32, 34, 36, 40, 42, 44, 46, 50, 52, 54, 56, 60, 62, 64, 66)
val billions: MutableList<Int> = mutableListOf(0, 2, 4, 6)
fun ebanNumbers(start: Int, stop: Int): MutableList<Int> {
    var nums: MutableList<Int> = mutableListOf<Int>()
    for (b in billions) {
        for (m in vals) {
            for (t in vals) {
                for (r in vals) {
                    val n: BigInteger = ((((b * 1000000000) + (m * 1000000)) + (t * 1000)) + r).toBigInteger()
                    if ((n.compareTo(start.toBigInteger()) >= 0) && (n.compareTo(stop.toBigInteger()) <= 0)) {
                        nums = run { val _tmp = nums.toMutableList(); _tmp.add(n.toInt()); _tmp } as MutableList<Int>
                    }
                }
            }
        }
    }
    return nums
}

fun countEban(start: Int, stop: Int): Int {
    var count: Int = 0
    for (b in billions) {
        for (m in vals) {
            for (t in vals) {
                for (r in vals) {
                    val n: BigInteger = ((((b * 1000000000) + (m * 1000000)) + (t * 1000)) + r).toBigInteger()
                    if ((n.compareTo(start.toBigInteger()) >= 0) && (n.compareTo(stop.toBigInteger()) <= 0)) {
                        count = count + 1
                    }
                }
            }
        }
    }
    return count
}

fun user_main(): Unit {
    val ranges = mutableListOf(mutableListOf(2, 1000, true), mutableListOf(1000, 4000, true), mutableListOf(2, 10000, false), mutableListOf(2, 100000, false), mutableListOf(2, 1000000, false), mutableListOf(2, 10000000, false), mutableListOf(2, 100000000, false), mutableListOf(2, 1000000000, false))
    for (rg in ranges) {
        val start: Int = rg[0]
        val stop: Int = rg[1]
        val show: Boolean = (rg[2]) as bool
        if (start == 2) {
            println(("eban numbers up to and including " + stop.toString()) + ":")
        } else {
            println(((("eban numbers between " + start.toString()) + " and ") + stop.toString()) + " (inclusive):")
        }
        if (show as Boolean) {
            val nums: MutableList<Int> = ebanNumbers(start, stop)
            var line: String = ""
            var i: Int = 0
            while (i < nums.size) {
                line = (line + (nums[i]).toString()) + " "
                i = i + 1
            }
            if (line.length > 0) {
                println(line.substring(0, line.length - 1))
            }
        }
        val c: Int = countEban(start, stop)
        println(("count = " + c.toString()) + "\n")
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
