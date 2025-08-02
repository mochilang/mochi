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

fun sel(list: MutableList<Double>, k: Int): Double {
    var i: Int = 0
    while (i <= k) {
        var minIndex: Int = i
        var j: BigInteger = (i + 1).toBigInteger()
        while (j.compareTo((list.size).toBigInteger()) < 0) {
            if (list[(j).toInt()]!! < list[minIndex]!!) {
                minIndex = j.toInt()
            }
            j = j.add((1).toBigInteger())
        }
        var tmp: Double = list[i]!!
        list[i] = list[minIndex]!!
        list[minIndex] = tmp
        i = i + 1
    }
    return list[k]!!
}

fun median(a: MutableList<Double>): Double {
    var arr: MutableList<Double> = a
    var half: Int = (arr.size / 2).toInt()
    var med: Double = sel(arr, half)
    if ((Math.floorMod(arr.size, 2)) == 0) {
        return (med + arr[half - 1]!!) / 2.0
    }
    return med
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println(median(mutableListOf(3.0, 1.0, 4.0, 1.0)).toString())
        println(median(mutableListOf(3.0, 1.0, 4.0, 1.0, 5.0)).toString())
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
