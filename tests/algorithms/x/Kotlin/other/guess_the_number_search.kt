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

fun get_avg(number_1: Int, number_2: Int): Int {
    return (number_1 + number_2) / 2
}

fun guess_the_number(lower: Int, higher: Int, to_guess: Int): MutableList<Int> {
    if (lower > higher) {
        panic("argument value for lower and higher must be(lower > higher)")
    }
    if (!(((lower < to_guess) && (to_guess < higher)) as Boolean)) {
        panic("guess value must be within the range of lower and higher value")
    }
    fun answer(number: Int): String {
        if (number > to_guess) {
            return "high"
        } else {
            if (number < to_guess) {
                return "low"
            } else {
                return "same"
            }
        }
    }

    println("started...")
    var last_lowest: Int = (lower).toInt()
    var last_highest: Int = (higher).toInt()
    var last_numbers: MutableList<Int> = mutableListOf<Int>()
    while (true) {
        var number: Int = (get_avg((last_lowest.toInt()), last_highest)).toInt()
        last_numbers = run { val _tmp = last_numbers.toMutableList(); _tmp.add(number); _tmp }
        var resp: String = ((answer(number)) as String)
        if (resp == "low") {
            last_lowest = number
        } else {
            if (resp == "high") {
                last_highest = number
            } else {
                break
            }
        }
    }
    println("guess the number : " + (last_numbers[last_numbers.size - 1]!!).toString())
    println("details : " + last_numbers.toString())
    return last_numbers
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        guess_the_number(10, 1000, 17)
        guess_the_number(0 - 10000, 10000, 7)
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
