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

var daysInMonth: MutableList<Int> = mutableListOf(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
var start: MutableList<Int> = mutableListOf(3, 6, 6, 2, 4, 0, 2, 5, 1, 3, 6, 1)
var months: MutableList<String> = mutableListOf(" January ", " February", "  March  ", "  April  ", "   May   ", "   June  ", "   July  ", "  August ", "September", " October ", " November", " December")
var days: MutableList<String> = mutableListOf("Su", "Mo", "Tu", "We", "Th", "Fr", "Sa")
fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println("                                [SNOOPY]\n")
        println("                                  1969\n")
        var qtr: Int = 0
        while (qtr < 4) {
            var mi: Int = 0
            while (mi < 3) {
                print(("      " + months[(qtr * 3) + mi]!!) + "           ")
                mi = mi + 1
            }
            println("")
            mi = 0
            while (mi < 3) {
                var d: Int = 0
                while (d < 7) {
                    print(" " + days[d]!!)
                    d = d + 1
                }
                print("     ")
                mi = mi + 1
            }
            println("")
            var week: Int = 0
            while (week < 6) {
                mi = 0
                while (mi < 3) {
                    var day: Int = 0
                    while (day < 7) {
                        var m: BigInteger = ((qtr * 3) + mi).toBigInteger()
                        var _val: BigInteger = ((((week * 7) + day) - start[(m).toInt()]!!) + 1).toBigInteger()
                        if ((_val.compareTo((1).toBigInteger()) >= 0) && (_val.compareTo((daysInMonth[(m).toInt()]!!).toBigInteger()) <= 0)) {
                            var s: String = _val.toString()
                            if (s.length == 1) {
                                s = " " + s
                            }
                            print(" " + s)
                        } else {
                            print("   ")
                        }
                        day = day + 1
                    }
                    print("     ")
                    mi = mi + 1
                }
                println("")
                week = week + 1
            }
            println("")
            qtr = qtr + 1
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
