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
        _nowSeed.toInt()
    } else {
        System.nanoTime().toInt()
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

fun shuffle(xs: MutableList<Int>): MutableList<Int> {
    var arr: MutableList<Int> = xs
    var i: Int = 99
    while (i > 0) {
        val j: Int = _now() % (i + 1)
        val tmp: Int = arr[i]
        arr[i] = arr[j]
        arr[j] = tmp
        i = i - 1
    }
    return arr
}

fun doTrials(trials: Int, np: Int, strategy: String): Unit {
    var pardoned: Int = 0
    var t: Int = 0
    while (t < trials) {
        var drawers: MutableList<Int> = mutableListOf()
        var i: Int = 0
        while (i < 100) {
            drawers = run { val _tmp = drawers.toMutableList(); _tmp.add(i); _tmp } as MutableList<Int>
            i = i + 1
        }
        drawers = shuffle(drawers)
        var p: Int = 0
        var success: Boolean = true
        while (p < np) {
            var found: Boolean = false
            if (strategy == "optimal") {
                var prev: Int = p
                var d: Int = 0
                while (d < 50) {
                    val _this: Int = drawers[prev]
                    if (_this == p) {
                        found = true
                        break
                    }
                    prev = _this
                    d = d + 1
                }
            } else {
                var opened: MutableList<Boolean> = mutableListOf()
                var k: Int = 0
                while (k < 100) {
                    opened = run { val _tmp = opened.toMutableList(); _tmp.add(false); _tmp } as MutableList<Boolean>
                    k = k + 1
                }
                var d: Int = 0
                while (d < 50) {
                    var n: Int = _now() % 100
                    while ((opened[n]) as Boolean) {
                        n = _now() % 100
                    }
                    opened[n] = true
                    if (drawers[n] == p) {
                        found = true
                        break
                    }
                    d = d + 1
                }
            }
            if (!found) {
                success = false
                break
            }
            p = p + 1
        }
        if (success as Boolean) {
            pardoned = pardoned + 1
        }
        t = t + 1
    }
    val rf: Double = (pardoned.toDouble() / trials.toDouble()) * 100.0
    println(((((("  strategy = " + strategy) + "  pardoned = ") + pardoned.toString()) + " relative frequency = ") + rf.toString()) + "%")
}

fun user_main(): Unit {
    val trials: Int = 1000
    for (np in mutableListOf(10, 100)) {
        println(((("Results from " + trials.toString()) + " trials with ") + np.toString()) + " prisoners:\n")
        for (strat in mutableListOf("random", "optimal")) {
            doTrials(trials, np, strat)
        }
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
