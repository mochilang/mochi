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
    val square1: Int = a + b
    val square2: Int = (b + c) + d
    val square3: Int = (d + e) + f
    val square4: Int = f + g
    return ((((square1 == square2) && (square2 == square3) as Boolean)) && (square3 == square4)) as Boolean
}

fun isUnique(a: Int, b: Int, c: Int, d: Int, e: Int, f: Int, g: Int): Boolean {
    var nums: MutableList<Int> = mutableListOf(a, b, c, d, e, f, g)
    var i: Int = 0
    while (i < nums.size) {
        var j: Int = i + 1
        while (j < nums.size) {
            if (nums[i] == nums[j]) {
                return false
            }
            j = j + 1
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
                val s: Int = (b + c) + d
                for (e in low until high + 1) {
                    for (f in low until high + 1) {
                        val a: Int = s - b
                        val g: Int = s - f
                        if ((a < low) || (a > high)) {
                            continue
                        }
                        if ((g < low) || (g > high)) {
                            continue
                        }
                        if (((d + e) + f) != s) {
                            continue
                        }
                        if ((f + g) != s) {
                            continue
                        }
                        if ((!unique as Boolean) || isUnique(a, b, c, d, e, f, g)) {
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
