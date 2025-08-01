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

fun image(): MutableList<MutableList<Int>> {
    return mutableListOf(mutableListOf(0, 0, 10000), mutableListOf(65535, 65535, 65535), mutableListOf(65535, 65535, 65535))
}

fun histogram(g: MutableList<MutableList<Int>>, bins: Int): MutableList<Int> {
    var bins: Int = bins
    if (bins <= 0) {
        bins = (g[0]!!).size
    }
    var h: MutableList<Int> = mutableListOf<Int>()
    var i: Int = 0
    while (i < bins) {
        h = run { val _tmp = h.toMutableList(); _tmp.add(0); _tmp } as MutableList<Int>
        i = i + 1
    }
    var y: Int = 0
    while (y < g.size) {
        var row: MutableList<Int> = g[y]!!
        var x: Int = 0
        while (x < row.size) {
            var p: Int = row[x]!!
            var idx: Int = ((p * (bins - 1)) / 65535).toInt()
            (h[idx]) = h[idx]!! + 1
            x = x + 1
        }
        y = y + 1
    }
    return h
}

fun medianThreshold(h: MutableList<Int>): Int {
    var lb: Int = 0
    var ub: BigInteger = (h.size - 1).toBigInteger()
    var lSum: Int = 0
    var uSum: Int = 0
    while ((lb).toBigInteger().compareTo(ub) <= 0) {
        if ((lSum + h[lb]!!) < (uSum + h[(ub).toInt()]!!)) {
            lSum = lSum + h[lb]!!
            lb = lb + 1
        } else {
            uSum = uSum + h[(ub).toInt()]!!
            ub = ub.subtract(1.toBigInteger())
        }
    }
    return ((ub.multiply(65535.toBigInteger())).divide(h.size.toBigInteger())).toInt()
}

fun threshold(g: MutableList<MutableList<Int>>, t: Int): MutableList<MutableList<Int>> {
    var out: MutableList<MutableList<Int>> = mutableListOf<MutableList<Int>>()
    var y: Int = 0
    while (y < g.size) {
        var row: MutableList<Int> = g[y]!!
        var newRow: MutableList<Int> = mutableListOf<Int>()
        var x: Int = 0
        while (x < row.size) {
            if (row[x]!! < t) {
                newRow = run { val _tmp = newRow.toMutableList(); _tmp.add(0); _tmp } as MutableList<Int>
            } else {
                newRow = run { val _tmp = newRow.toMutableList(); _tmp.add(65535); _tmp } as MutableList<Int>
            }
            x = x + 1
        }
        out = run { val _tmp = out.toMutableList(); _tmp.add(newRow); _tmp } as MutableList<MutableList<Int>>
        y = y + 1
    }
    return out
}

fun printImage(g: MutableList<MutableList<Int>>): Unit {
    var y: Int = 0
    while (y < g.size) {
        var row: MutableList<Int> = g[y]!!
        var line: String = ""
        var x: Int = 0
        while (x < row.size) {
            if (row[x]!! == 0) {
                line = line + "0"
            } else {
                line = line + "1"
            }
            x = x + 1
        }
        println(line)
        y = y + 1
    }
}

fun user_main(): Unit {
    var img: MutableList<MutableList<Int>> = image()
    var h: MutableList<Int> = histogram(img, 0)
    println("Histogram: " + h.toString())
    var t: Int = medianThreshold(h)
    println("Threshold: " + t.toString())
    var bw: MutableList<MutableList<Int>> = threshold(img, t)
    printImage(bw)
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
