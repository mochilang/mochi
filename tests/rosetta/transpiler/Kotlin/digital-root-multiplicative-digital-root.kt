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

data class MDRResult(var mp: Int, var mdr: Int)
fun pad(s: String, width: Int): String {
    var out: String = s
    while (out.length < width) {
        out = " " + out
    }
    return out
}

fun mult(n: BigInteger, base: Int): BigInteger {
    var m: BigInteger = java.math.BigInteger.valueOf(1)
    var x: BigInteger = n
    val b: BigInteger = base.toBigInteger()
    while (x.compareTo(0.toBigInteger()) > 0) {
        m = m.multiply((x.remainder(b)))
        x = x.divide(b)
    }
    return m
}

fun multDigitalRoot(n: BigInteger, base: Int): MDRResult {
    var m: BigInteger = n
    var mp: Int = 0
    val b: BigInteger = base.toBigInteger()
    while (m.compareTo(b) >= 0) {
        m = mult(m, base)
        mp = mp + 1
    }
    return MDRResult(mp = mp, mdr = m.toInt())
}

fun user_main(): Unit {
    val base: Int = 10
    val size: Int = 5
    println((((pad("Number", 20) + " ") + pad("MDR", 3)) + " ") + pad("MP", 3))
    val nums: MutableList<BigInteger> = mutableListOf(123321.toBigInteger(), 7739.toBigInteger(), 893.toBigInteger(), 899998.toBigInteger(), 3778888999L.toBigInteger(), 277777788888899L.toBigInteger())
    var i: Int = 0
    while (i < nums.size) {
        val n: BigInteger = nums[i]
        val r: MDRResult = multDigitalRoot(n, base)
        println((((pad(n.toString(), 20) + " ") + pad(r.mdr.toString(), 3)) + " ") + pad(r.mp.toString(), 3))
        i = i + 1
    }
    println("")
    var list: MutableList<MutableList<Int>> = mutableListOf<MutableList<Int>>()
    var idx: Int = 0
    while (idx < base) {
        list = run { val _tmp = list.toMutableList(); _tmp.add(mutableListOf<Int>()); _tmp } as MutableList<MutableList<Int>>
        idx = idx + 1
    }
    var cnt: BigInteger = (size * base).toBigInteger()
    var n: BigInteger = java.math.BigInteger.valueOf(0)
    val b: BigInteger = base.toBigInteger()
    while (cnt.compareTo(0.toBigInteger()) > 0) {
        val r: MDRResult = multDigitalRoot(n, base)
        val mdr: Int = r.mdr
        if ((list[mdr]).size < size) {
            list[mdr] = run { val _tmp = (list[mdr]).toMutableList(); _tmp.add(n.toInt()); _tmp } as MutableList<Int>
            cnt = cnt.subtract(1.toBigInteger())
        }
        n = n.add(1.toBigInteger())
    }
    println("MDR: First")
    var j: Int = 0
    while (j < base) {
        println((pad(j.toString(), 3) + ": ") + (list[j]).toString())
        j = j + 1
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
