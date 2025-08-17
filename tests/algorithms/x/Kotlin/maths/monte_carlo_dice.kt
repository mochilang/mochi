import java.math.BigInteger

fun <T> _listSet(lst: MutableList<T>, idx: Int, v: T) { while (lst.size <= idx) lst.add(v); lst[idx] = v }

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

var lcg_seed: Int = (1).toInt()
fun lcg_rand(): Int {
    lcg_seed = ((Math.floorMod((((lcg_seed * 1103515245) + 12345).toLong()), 2147483648L)).toInt()).toInt()
    return lcg_seed
}

fun roll(): Int {
    var rv: Double = (lcg_rand()).toDouble()
    var r: Double = (rv * 6.0) / 2147483648.0
    return 1 + (r.toInt())
}

fun round2(x: Double): Double {
    var y: Double = (x * 100.0) + 0.5
    var z: Int = (y.toInt()).toInt()
    return (z.toDouble()) / 100.0
}

fun throw_dice(num_throws: Int, num_dice: Int): MutableList<Double> {
    var count_of_sum: MutableList<Int> = mutableListOf<Int>()
    var max_sum: Int = ((num_dice * 6) + 1).toInt()
    var i: Int = (0).toInt()
    while (i < max_sum) {
        count_of_sum = run { val _tmp = count_of_sum.toMutableList(); _tmp.add(0); _tmp }
        i = i + 1
    }
    var t: Int = (0).toInt()
    while (t < num_throws) {
        var s: Int = (0).toInt()
        var d: Int = (0).toInt()
        while (d < num_dice) {
            s = s + roll()
            d = d + 1
        }
        _listSet(count_of_sum, s, count_of_sum[s]!! + 1)
        t = t + 1
    }
    var probability: MutableList<Double> = mutableListOf<Double>()
    i = num_dice
    while (i < max_sum) {
        var p: Double = (((count_of_sum[i]!!).toDouble()) * 100.0) / (num_throws.toDouble())
        probability = run { val _tmp = probability.toMutableList(); _tmp.add(round2(p)); _tmp }
        i = i + 1
    }
    return probability
}

fun user_main(): Unit {
    lcg_seed = (1).toInt()
    var result: MutableList<Double> = throw_dice(10000, 2)
    println(result.toString())
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
