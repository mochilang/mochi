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

fun randOrder(seed: Int, n: Int): MutableList<Int> {
    val next: Int = Math.floorMod(((seed * 1664525) + 1013904223), 2147483647)
    return mutableListOf(next, Math.floorMod(next, n))
}

fun randChaos(seed: Int, n: Int): MutableList<Int> {
    val next: Int = Math.floorMod(((seed * 1103515245) + 12345), 2147483647)
    return mutableListOf(next, Math.floorMod(next, n))
}

fun user_main(): Unit {
    val nBuckets: Int = 10
    val initialSum: Int = 1000
    var buckets: MutableList<Int> = mutableListOf()
    for (i in 0 until nBuckets) {
        buckets = run { val _tmp = buckets.toMutableList(); _tmp.add(0); _tmp } as MutableList<Int>
    }
    var i: Int = nBuckets
    var dist: Int = initialSum
    while (i > 0) {
        val v: Int = dist / i
        i = i - 1
        buckets[i] = v
        dist = dist - v
    }
    var tc0: Int = 0
    var tc1: Int = 0
    var total: Int = 0
    var nTicks: Int = 0
    var seedOrder: Int = 1
    var seedChaos: Int = 2
    println("sum  ---updates---    mean  buckets")
    var t: Int = 0
    while (t < 5) {
        var r: MutableList<Int> = randOrder(seedOrder, nBuckets)
        seedOrder = r[0]
        var b1: Int = r[1]
        var b2: BigInteger = (Math.floorMod((b1 + 1), nBuckets)).toBigInteger()
        val v1: Int = buckets[b1]
        val v2: Int = (buckets)[(b2).toInt()] as Int
        if (v1 > v2) {
            var a: Int = ((v1 - v2) / 2).toInt()
            if (a > buckets[b1]) {
                a = buckets[b1]
            }
            buckets[b1] = buckets[b1] - a
            buckets[(b2).toInt()] = (buckets)[(b2).toInt()] as Int + a
        } else {
            var a: Int = ((v2 - v1) / 2).toInt()
            if (a > (buckets)[(b2).toInt()] as Int) {
                a = (buckets)[(b2).toInt()] as Int
            }
            buckets[(b2).toInt()] = (buckets)[(b2).toInt()] as Int - a
            buckets[b1] = buckets[b1] + a
        }
        tc0 = tc0 + 1
        r = randChaos(seedChaos, nBuckets)
        seedChaos = r[0]
        b1 = r[1]
        b2 = (Math.floorMod((b1 + 1), nBuckets)).toBigInteger()
        r = randChaos(seedChaos, buckets[b1] + 1)
        seedChaos = r[0]
        var amt: Int = r[1]
        if (amt > buckets[b1]) {
            amt = buckets[b1]
        }
        buckets[b1] = buckets[b1] - amt
        buckets[(b2).toInt()] = (buckets)[(b2).toInt()] as Int + amt
        tc1 = tc1 + 1
        var sum: Int = 0
        var idx: Int = 0
        while (idx < nBuckets) {
            sum = sum + buckets[idx]
            idx = idx + 1
        }
        total = (total + tc0) + tc1
        nTicks = nTicks + 1
        println((((((((sum.toString() + " ") + tc0.toString()) + " ") + tc1.toString()) + " ") + (total / nTicks).toString()) + "  ") + buckets.toString())
        tc0 = 0
        tc1 = 0
        t = t + 1
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
