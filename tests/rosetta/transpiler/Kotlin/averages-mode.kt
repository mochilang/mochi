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

var arr1: MutableList<Int> = mutableListOf(2, 7, 1, 8, 2)
var counts1: MutableMap<Int, Int> = mutableMapOf<Int, Int>()
var keys1: MutableList<Int> = mutableListOf<Int>()
var i: Int = 0
var max1: Int = 0
fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        while (i < arr1.size) {
            var v: Int = arr1[i]!!
            if (v in counts1) {
                (counts1)[v] = (counts1)[v] as Int + 1
            } else {
                (counts1)[v] = 1
                keys1 = run { val _tmp = keys1.toMutableList(); _tmp.add(v); _tmp } as MutableList<Int>
            }
            i = i + 1
        }
        i = 0
        while (i < keys1.size) {
            var k: Int = keys1[i]!!
            var c: Int = (counts1)[k] as Int
            if (c > max1) {
                max1 = c
            }
            i = i + 1
        }
        var modes1: MutableList<Int> = mutableListOf<Int>()
        i = 0
        while (i < keys1.size) {
            var k: Int = keys1[i]!!
            if ((counts1)[k] as Int == max1) {
                modes1 = run { val _tmp = modes1.toMutableList(); _tmp.add(k); _tmp } as MutableList<Int>
            }
            i = i + 1
        }
        println(modes1.toString())
        var arr2: MutableList<Int> = mutableListOf(2, 7, 1, 8, 2, 8)
        var counts2: MutableMap<Int, Int> = mutableMapOf<Int, Int>()
        var keys2: MutableList<Int> = mutableListOf<Int>()
        i = 0
        while (i < arr2.size) {
            var v: Int = arr2[i]!!
            if (v in counts2) {
                (counts2)[v] = (counts2)[v] as Int + 1
            } else {
                (counts2)[v] = 1
                keys2 = run { val _tmp = keys2.toMutableList(); _tmp.add(v); _tmp } as MutableList<Int>
            }
            i = i + 1
        }
        var max2: Int = 0
        i = 0
        while (i < keys2.size) {
            var k: Int = keys2[i]!!
            var c: Int = (counts2)[k] as Int
            if (c > max2) {
                max2 = c
            }
            i = i + 1
        }
        var modes2: MutableList<Int> = mutableListOf<Int>()
        i = 0
        while (i < keys2.size) {
            var k: Int = keys2[i]!!
            if ((counts2)[k] as Int == max2) {
                modes2 = run { val _tmp = modes2.toMutableList(); _tmp.add(k); _tmp } as MutableList<Int>
            }
            i = i + 1
        }
        println(modes2.toString())
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
