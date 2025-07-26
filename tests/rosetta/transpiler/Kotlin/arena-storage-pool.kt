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

fun poolPut(p: MutableList<Int>, x: Int): MutableList<Int> {
    return run { val _tmp = p.toMutableList(); _tmp.add(x); _tmp } as MutableList<Int>
}

fun poolGet(p: MutableList<Int>): MutableMap<String, Any?> {
    var p: MutableList<Int> = p
    if (p.size == 0) {
        println("pool empty")
        return mutableMapOf<String, Any?>("pool" to (p), "val" to (0))
    }
    val idx: Int = p.size - 1
    val v: Int = p[idx]
    p = p.subList(0, idx)
    return mutableMapOf<String, Any?>("pool" to (p), "val" to (v))
}

fun clearPool(p: MutableList<Int>): MutableList<Int> {
    return mutableListOf<Int>()
}

fun user_main(): Unit {
    var pool: MutableList<Int> = mutableListOf()
    var i: Int = 1
    var j: Int = 2
    println((i + j).toString())
    pool = poolPut(pool, i)
    pool = poolPut(pool, j)
    i = 0
    j = 0
    val res1: MutableMap<String, Any?> = poolGet(pool)
    pool = ((res1)["pool"] as Any?) as MutableList<Int>
    i = (res1)["val"] as Int
    val res2: MutableMap<String, Any?> = poolGet(pool)
    pool = ((res2)["pool"] as Any?) as MutableList<Int>
    j = (res2)["val"] as Int
    i = 4
    j = 5
    println((i + j).toString())
    pool = poolPut(pool, i)
    pool = poolPut(pool, j)
    i = 0
    j = 0
    pool = clearPool(pool)
    val res3: MutableMap<String, Any?> = poolGet(pool)
    pool = ((res3)["pool"] as Any?) as MutableList<Int>
    i = (res3)["val"] as Int
    val res4: MutableMap<String, Any?> = poolGet(pool)
    pool = ((res4)["pool"] as Any?) as MutableList<Int>
    j = (res4)["val"] as Int
    i = 7
    j = 8
    println((i + j).toString())
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
