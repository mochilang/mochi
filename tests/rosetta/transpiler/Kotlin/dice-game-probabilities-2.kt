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

fun roll(nDice: Int, nSides: Int): Int {
    var sum: Int = 0
    var i: Int = 0
    while (i < nDice) {
        sum = (sum + (Math.floorMod(_now(), nSides))) + 1
        i = i + 1
    }
    return sum
}

fun beats(n1: Int, s1: Int, n2: Int, s2: Int, trials: Int): Double {
    var wins: Int = 0
    var i: Int = 0
    while (i < trials) {
        if (roll(n1, s1) > roll(n2, s2)) {
            wins = wins + 1
        }
        i = i + 1
    }
    return ((wins.toDouble())) / ((trials.toDouble()))
}

fun main() {
    println(beats(9, 4, 6, 6, 1000).toString())
    println(beats(5, 10, 7, 6, 1000).toString())
}
