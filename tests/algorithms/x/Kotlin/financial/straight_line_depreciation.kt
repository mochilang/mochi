fun panic(msg: String): Nothing { throw RuntimeException(msg) }

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

fun straight_line_depreciation(useful_years: Int, purchase_value: Double, residual_value: Double): MutableList<Double> {
    if (useful_years < 1) {
        panic("Useful years cannot be less than 1")
    }
    if (purchase_value < 0.0) {
        panic("Purchase value cannot be less than zero")
    }
    if (purchase_value < residual_value) {
        panic("Purchase value cannot be less than residual value")
    }
    var depreciable_cost: Double = purchase_value - residual_value
    var annual_expense: Double = depreciable_cost / (1.0 * (useful_years).toDouble())
    var expenses: MutableList<Double> = mutableListOf<Double>()
    var accumulated: Double = 0.0
    var period: Int = (0).toInt()
    while (period < useful_years) {
        if (period != (useful_years - 1)) {
            accumulated = accumulated + annual_expense
            expenses = run { val _tmp = expenses.toMutableList(); _tmp.add(annual_expense); _tmp }
        } else {
            var end_year_expense: Double = depreciable_cost - accumulated
            expenses = run { val _tmp = expenses.toMutableList(); _tmp.add(end_year_expense); _tmp }
        }
        period = period + 1
    }
    return expenses
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println(straight_line_depreciation(10, 1100.0, 100.0).toString())
        println(straight_line_depreciation(6, 1250.0, 50.0).toString())
        println(straight_line_depreciation(4, 1001.0, 0.0).toString())
        println(straight_line_depreciation(11, 380.0, 50.0).toString())
        println(straight_line_depreciation(1, 4985.0, 100.0).toString())
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
