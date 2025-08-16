import java.math.BigInteger

fun <T> _listSet(lst: MutableList<T>, idx: Int, v: T) { while (lst.size <= idx) lst.add(v); lst[idx] = v }

fun _numToStr(v: Number): String {
    val d = v.toDouble()
    val i = d.toLong()
    return if (d == i.toDouble()) i.toString() else d.toString()
}

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

var tableau: MutableList<MutableList<Double>> = mutableListOf(mutableListOf(0.0 - 1.0, 0.0 - 1.0, 0.0, 0.0, 0.0), mutableListOf(1.0, 3.0, 1.0, 0.0, 4.0), mutableListOf(3.0, 1.0, 0.0, 1.0, 4.0))
var finalTab: MutableList<MutableList<Double>> = simplex(tableau)
var res: MutableMap<String, Double> = interpret(finalTab, 2)
fun pivot(t: MutableList<MutableList<Double>>, row: Int, col: Int): MutableList<MutableList<Double>> {
    var pivotRow: MutableList<Double> = mutableListOf<Double>()
    var pivotVal: Double = (((t[row]!!) as MutableList<Double>))[col]!!
    for (j in 0 until (t[row]!!).size) {
        pivotRow = run { val _tmp = pivotRow.toMutableList(); _tmp.add((((t[row]!!) as MutableList<Double>))[j]!! / pivotVal); _tmp }
    }
    _listSet(t, row, pivotRow)
    for (i in 0 until t.size) {
        if (i != row) {
            var factor: Double = (((t[i]!!) as MutableList<Double>))[col]!!
            var newRow: MutableList<Double> = mutableListOf<Double>()
            for (j in 0 until (t[i]!!).size) {
                var value: Double = (((t[i]!!) as MutableList<Double>))[j]!! - (factor * pivotRow[j]!!)
                newRow = run { val _tmp = newRow.toMutableList(); _tmp.add(value); _tmp }
            }
            _listSet(t, i, newRow)
        }
    }
    return t
}

fun findPivot(t: MutableList<MutableList<Double>>): MutableList<Int> {
    var col: Int = (0).toInt()
    var minVal: Double = 0.0
    for (j in 0 until (t[0]!!).size - 1) {
        var v: Double = (((t[0]!!) as MutableList<Double>))[j]!!
        if (v < minVal) {
            minVal = v
            col = j
        }
    }
    if (minVal >= 0.0) {
        return mutableListOf(0 - 1, 0 - 1)
    }
    var row: Int = (0 - 1).toInt()
    var minRatio: Double = 0.0
    var first: Boolean = true
    for (i in 1 until t.size) {
        var coeff: Double = (((t[i]!!) as MutableList<Double>))[col]!!
        if (coeff > 0.0) {
            var rhs: Double = (((t[i]!!) as MutableList<Double>))[(t[i]!!).size - 1]!!
            var ratio: Double = rhs / coeff
            if (first || (ratio < minRatio)) {
                minRatio = ratio
                row = i
                first = false
            }
        }
    }
    return mutableListOf(row, col)
}

fun interpret(t: MutableList<MutableList<Double>>, nVars: Int): MutableMap<String, Double> {
    var lastCol: Int = ((t[0]!!).size - 1).toInt()
    var p: Double = (((t[0]!!) as MutableList<Double>))[lastCol]!!
    if (p < 0.0) {
        p = 0.0 - p
    }
    var result: MutableMap<String, Double> = mutableMapOf<String, Double>()
    (result)["P"] = p
    for (i in 0 until nVars) {
        var nzRow: Int = (0 - 1).toInt()
        var nzCount: Int = (0).toInt()
        for (r in 0 until t.size) {
            var _val: Double = (((t[r]!!) as MutableList<Double>))[i]!!
            if (_val != 0.0) {
                nzCount = nzCount + 1
                nzRow = r
            }
        }
        if ((nzCount == 1) && ((((t[nzRow]!!) as MutableList<Double>))[i]!! == 1.0)) {
            (result)["x" + _numToStr(i + 1)] = (((t[nzRow]!!) as MutableList<Double>))[lastCol]!!
        }
    }
    return result
}

fun simplex(tab: MutableList<MutableList<Double>>): MutableList<MutableList<Double>> {
    var t: MutableList<MutableList<Double>> = tab
    while (true) {
        var p: MutableList<Int> = findPivot(t)
        var row: Int = (p[0]!!).toInt()
        var col: Int = (p[1]!!).toInt()
        if (row < 0) {
            break
        }
        t = pivot(t, row, col)
    }
    return t
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println("P: " + _numToStr((res)["P"] as Double))
        for (i in 0 until 2) {
            var key: String = "x" + _numToStr(i + 1)
            if (key in res) {
                println((key + ": ") + _numToStr((res)[key] as Double))
            }
        }
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
