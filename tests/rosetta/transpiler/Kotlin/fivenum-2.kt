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

var x1: MutableList<Double> = mutableListOf(36.0, 40.0, 7.0, 39.0, 41.0, 15.0)
var x2: MutableList<Double> = mutableListOf(15.0, 6.0, 42.0, 41.0, 7.0, 36.0, 49.0, 40.0, 39.0, 47.0, 43.0)
var x3: MutableList<Double> = mutableListOf(0.14082834, 0.0974879, 1.73131507, 0.87636009, 0.0 - 1.95059594, 0.73438555, 0.0 - 0.03035726, 1.4667597, 0.0 - 0.74621349, 0.0 - 0.72588772, 0.6390516, 0.61501527, 0.0 - 0.9898378, 0.0 - 1.00447874, 0.0 - 0.62759469, 0.66206163, 1.04312009, 0.0 - 0.10305385, 0.75775634, 0.32566578)
fun qsel(a: MutableList<Double>, k: Int): Double {
    var k: Int = k
    var arr: MutableList<Double> = a
    while (arr.size > 1) {
        var px: BigInteger = (Math.floorMod(_now(), arr.size)).toBigInteger()
        var pv: Double = arr[(px).toInt()]!!
        var last: BigInteger = (arr.size - 1).toBigInteger()
        var tmp: Double = arr[(px).toInt()]!!
        (arr[(px).toInt()]) = arr[(last).toInt()]!!
        (arr[(last).toInt()]) = tmp
        px = 0.toBigInteger()
        var i: Int = 0
        while ((i).toBigInteger().compareTo(last) < 0) {
            var v: Double = arr[i]!!
            if (v < pv) {
                var t: Double = arr[(px).toInt()]!!
                (arr[(px).toInt()]) = arr[i]!!
                (arr[i]) = t
                px = px.add(1.toBigInteger())
            }
            i = i + 1
        }
        (arr[(px).toInt()]) = pv
        if (px.compareTo(k.toBigInteger()) == 0) {
            return pv
        }
        if ((k).toBigInteger().compareTo(px) < 0) {
            arr = arr.subList(0, (px).toInt())
        } else {
            arr = arr.subList((px.add(1.toBigInteger())).toInt(), arr.size)
            k = ((k).toBigInteger().subtract((px.add(1.toBigInteger())))).toInt()
        }
    }
    return arr[0]!!
}

fun fivenum(a: MutableList<Double>): MutableList<Double> {
    var last: BigInteger = (a.size - 1).toBigInteger()
    var m: BigInteger = last.divide(2.toBigInteger())
    var n5: MutableList<Double> = mutableListOf<Double>()
    n5 = run { val _tmp = n5.toMutableList(); _tmp.add(qsel(a.subList(0, (m).toInt()), 0)); _tmp } as MutableList<Double>
    n5 = run { val _tmp = n5.toMutableList(); _tmp.add(qsel(a.subList(0, (m).toInt()), a.size / 4)); _tmp } as MutableList<Double>
    n5 = run { val _tmp = n5.toMutableList(); _tmp.add(qsel(a, m.toInt())); _tmp } as MutableList<Double>
    var arr2: MutableList<Double> = a.subList((m).toInt(), a.size)
    var q3: BigInteger = (last.subtract(m)).subtract((a.size / 4).toBigInteger())
    n5 = run { val _tmp = n5.toMutableList(); _tmp.add(qsel(arr2, q3.toInt())); _tmp } as MutableList<Double>
    arr2 = arr2.subList((q3).toInt(), arr2.size)
    n5 = run { val _tmp = n5.toMutableList(); _tmp.add(qsel(arr2, arr2.size - 1)); _tmp } as MutableList<Double>
    return n5
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println(fivenum(x1).toString())
        println(fivenum(x2).toString())
        println(fivenum(x3).toString())
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
