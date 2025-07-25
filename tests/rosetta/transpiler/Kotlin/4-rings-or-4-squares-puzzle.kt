import java.math.BigInteger

var _nowSeed = 0L
var _nowSeeded = false
fun _now(): Int {
    if (!_nowSeeded) {
        System.getenv("MOCHI_NOW_SEED")?.toLongOrNull()?.let {
            _nowSeed = it
            _nowSeeded = true
        }
    }
    return if (_nowSeeded) {
        _nowSeed = (_nowSeed * 1664525 + 1013904223) % 2147483647
        kotlin.math.abs(_nowSeed.toInt())
    } else {
        kotlin.math.abs(System.nanoTime().toInt())
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

val r1: MutableMap<String, Any?> = getCombs(1, 7, true)
val r2: MutableMap<String, Any?> = getCombs(3, 9, true)
val r3: MutableMap<String, Any?> = getCombs(0, 9, false)
fun validComb(a: Int, b: Int, c: Int, d: Int, e: Int, f: Int, g: Int): Boolean {
    val square1: BigInteger = a + b
    val square2: BigInteger = (b + c) + d
    val square3: BigInteger = (d + e) + f
    val square4: BigInteger = f + g
    return ((((square1.compareTo(square2) == 0) && (square2.compareTo(square3) == 0) as Boolean)) && (square3.compareTo(square4) == 0)) as Boolean
}

fun isUnique(a: Int, b: Int, c: Int, d: Int, e: Int, f: Int, g: Int): Boolean {
    var nums: MutableList<Int> = mutableListOf(a, b, c, d, e, f, g)
    var i: Int = 0
    while (i < nums.size) {
        var j: BigInteger = i + 1
        while (j.compareTo(nums.size.toBigInteger()) < 0) {
            if (nums[i] == (nums)[(j).toInt()] as Int) {
                return false
            }
            j = j.add(1.toBigInteger())
        }
        i = i + 1
    }
    return true
}

fun getCombs(low: Int, high: Int, unique: Boolean): MutableMap<String, Any?> {
    var valid: MutableList<Any?> = mutableListOf()
    var count: Int = 0
    for (b in low until high + 1) {
        for (c in low until high + 1) {
            for (d in low until high + 1) {
                val s: BigInteger = (b + c) + d
                for (e in low until high + 1) {
                    for (f in low until high + 1) {
                        val a: BigInteger = s.subtract(b.toBigInteger())
                        val g: BigInteger = s.subtract(f.toBigInteger())
                        if ((a.compareTo(low.toBigInteger()) < 0) || (a.compareTo(high.toBigInteger()) > 0)) {
                            continue
                        }
                        if ((g.compareTo(low.toBigInteger()) < 0) || (g.compareTo(high.toBigInteger()) > 0)) {
                            continue
                        }
                        if (((d + e) + f).toBigInteger().compareTo(s) != 0) {
                            continue
                        }
                        if ((f.toBigInteger().add(g)).compareTo(s) != 0) {
                            continue
                        }
                        if ((!unique as Boolean) || isUnique(a as Int, b, c, d, e, f, g as Int)) {
                            valid = run { val _tmp = valid.toMutableList(); _tmp.add(mutableListOf(a, b, c, d, e, f, g)); _tmp } as MutableList<Any?>
                            count = count + 1
                        }
                    }
                }
            }
        }
    }
    return mutableMapOf<String, Any?>("count" to (count), "list" to (valid))
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println(((r1)["count"] as Any?).toString() + " unique solutions in 1 to 7")
        println((r1)["list"] as Any?)
        println(((r2)["count"] as Any?).toString() + " unique solutions in 3 to 9")
        println((r2)["list"] as Any?)
        println(((r3)["count"] as Any?).toString() + " non-unique solutions in 0 to 9")
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
