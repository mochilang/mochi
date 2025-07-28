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

val m: MutableList<MutableList<Double>> = mutableListOf(mutableListOf(2.0, 0.0 - 1.0, 5.0, 1.0), mutableListOf(3.0, 2.0, 2.0, 0.0 - 6.0), mutableListOf(1.0, 3.0, 3.0, 0.0 - 1.0), mutableListOf(5.0, 0.0 - 2.0, 0.0 - 3.0, 3.0))
val v: MutableList<Double> = mutableListOf(0.0 - 3.0, 0.0 - 32.0, 0.0 - 47.0, 49.0)
val d: Double = det(m)
var x: MutableList<Double> = mutableListOf<Double>()
var i: Int = 0
var s: String = "["
var j: Int = 0
fun det(m: MutableList<MutableList<Double>>): Double {
    val n: Int = m.size
    if (n == 1) {
        return m[0][0]
    }
    var total: Double = 0.0
    var sign: Double = 1.0
    var c: Int = 0
    while (c < n) {
        var sub: MutableList<MutableList<Double>> = mutableListOf<MutableList<Double>>()
        var r: Int = 1
        while (r < n) {
            var row: MutableList<Double> = mutableListOf<Double>()
            var cc: Int = 0
            while (cc < n) {
                if (cc != c) {
                    row = run { val _tmp = row.toMutableList(); _tmp.add(m[r][cc]); _tmp } as MutableList<Double>
                }
                cc = cc + 1
            }
            sub = run { val _tmp = sub.toMutableList(); _tmp.add(row); _tmp } as MutableList<MutableList<Double>>
            r = r + 1
        }
        total = total + ((sign * m[0][c]) * det(sub))
        sign = sign * (0.0 - 1.0)
        c = c + 1
    }
    return total
}

fun replaceCol(m: MutableList<MutableList<Double>>, col: Int, v: MutableList<Double>): MutableList<MutableList<Double>> {
    var res: MutableList<MutableList<Double>> = mutableListOf<MutableList<Double>>()
    var r: Int = 0
    while (r < m.size) {
        var row: MutableList<Double> = mutableListOf<Double>()
        var c: Int = 0
        while (c < (m[r]).size) {
            if (c == col) {
                row = run { val _tmp = row.toMutableList(); _tmp.add(v[r]); _tmp } as MutableList<Double>
            } else {
                row = run { val _tmp = row.toMutableList(); _tmp.add(m[r][c]); _tmp } as MutableList<Double>
            }
            c = c + 1
        }
        res = run { val _tmp = res.toMutableList(); _tmp.add(row); _tmp } as MutableList<MutableList<Double>>
        r = r + 1
    }
    return res
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        while (i < v.size) {
            val mc: MutableList<MutableList<Double>> = replaceCol(m, i, v)
            x = run { val _tmp = x.toMutableList(); _tmp.add(det(mc) / d); _tmp } as MutableList<Double>
            i = i + 1
        }
        while (j < x.size) {
            s = s + (x[j]).toString()
            if (j < (x.size - 1)) {
                s = s + " "
            }
            j = j + 1
        }
        s = s + "]"
        println(s)
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
