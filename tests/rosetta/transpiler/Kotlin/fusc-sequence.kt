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

fun fuscVal(n: Int): Int {
    var a: Int = 1
    var b: Int = 0
    var x: Int = n
    while (x > 0) {
        if ((Math.floorMod(x, 2)) == 0) {
            x = x / 2
            a = a + b
        } else {
            x = (x - 1) / 2
            b = a + b
        }
    }
    if (n == 0) {
        return 0
    }
    return b
}

fun firstFusc(n: Int): MutableList<Int> {
    var arr: MutableList<Int> = mutableListOf<Int>()
    var i: Int = 0
    while (i < n) {
        arr = run { val _tmp = arr.toMutableList(); _tmp.add(fuscVal(i)); _tmp } as MutableList<Int>
        i = i + 1
    }
    return arr
}

fun commatize(n: Int): String {
    var s: String = n.toString()
    var neg: Boolean = false
    if (n < 0) {
        neg = true
        s = s.substring(1, s.length) as String
    }
    var i: BigInteger = (s.length - 3).toBigInteger()
    while (i.compareTo(1.toBigInteger()) >= 0) {
        s = ((s.substring(0, (i).toInt())).toString() + ",") + (s.substring((i).toInt(), s.length)).toString()
        i = i.subtract(3.toBigInteger())
    }
    if (neg as Boolean) {
        return "-" + s
    }
    return s
}

fun padLeft(s: String, w: Int): String {
    var out: String = s
    while (out.length < w) {
        out = " " + out
    }
    return out
}

fun user_main(): Unit {
    println("The first 61 fusc numbers are:")
    println(firstFusc(61).toString())
    println("\nThe fusc numbers whose length > any previous fusc number length are:")
    var idxs: MutableList<Int> = mutableListOf(0, 37, 1173, 35499, 699051, 19573419)
    var i: Int = 0
    while (i < idxs.size) {
        var idx: Int = idxs[i]!!
        var _val: Int = fuscVal(idx)
        var numStr: String = padLeft(commatize(_val), 7)
        var idxStr: String = padLeft(commatize(idx), 10)
        println(((numStr + " (index ") + idxStr) + ")")
        i = i + 1
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
