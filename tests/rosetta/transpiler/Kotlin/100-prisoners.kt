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
        kotlin.math.abs(System.nanoTime().toInt())
    }
}

fun shuffle(xs: MutableList<Int>): MutableList<Int> {
    var arr: MutableList<Int> = xs
    var i: Int = 99
    while (i > 0) {
        val j: Int = _now() % (i + 1)
        val tmp: Int = (arr[i] as Int)
        arr[i] = (arr[j] as Int)
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
            drawers = (drawers + i).toMutableList()
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
                    val _this: Int = (drawers[prev] as Int)
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
                    opened = (opened + false).toMutableList()
                    k = k + 1
                }
                var d: Int = 0
                while (d < 50) {
                    var n: Int = _now() % 100
                    while ((opened[n] as Boolean) as Boolean) {
                        n = _now() % 100
                    }
                    opened[n] = true
                    if ((drawers[n] as Int) == p) {
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
    val rf: Double = ((pardoned.toDouble() as Number).toDouble() / (trials.toDouble() as Number).toDouble()) * 100.0
    println(((((("  strategy = " + strategy) + "  pardoned = ") + (pardoned.toString()).toString()) + " relative frequency = ") + (rf.toString()).toString()) + "%")
}

fun user_main(): Unit {
    val trials: Int = 1000
    for (np in mutableListOf(10, 100)) {
        println(((("Results from " + (trials.toString()).toString()) + " trials with ") + (np.toString()).toString()) + " prisoners:\n")
        for (strat in mutableListOf("random", "optimal")) {
            doTrials(trials, np, strat)
        }
    }
}

fun main() {
    user_main()
}
