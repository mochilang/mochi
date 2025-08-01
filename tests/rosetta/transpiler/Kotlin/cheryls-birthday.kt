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

data class Birthday(var month: Int, var day: Int)
var choices: MutableList<Birthday> = mutableListOf(Birthday(month = 5, day = 15), Birthday(month = 5, day = 16), Birthday(month = 5, day = 19), Birthday(month = 6, day = 17), Birthday(month = 6, day = 18), Birthday(month = 7, day = 14), Birthday(month = 7, day = 16), Birthday(month = 8, day = 14), Birthday(month = 8, day = 15), Birthday(month = 8, day = 17))
var filtered: MutableList<Birthday> = mutableListOf<Birthday>()
var filtered2: MutableList<Birthday> = mutableListOf<Birthday>()
var filtered3: MutableList<Birthday> = mutableListOf<Birthday>()
var filtered4: MutableList<Birthday> = mutableListOf<Birthday>()
fun monthUnique(b: Birthday, list: MutableList<Birthday>): Boolean {
    var c: Int = 0
    for (x in list) {
        if (x.month == b.month) {
            c = c + 1
        }
    }
    return c == 1
}

fun dayUnique(b: Birthday, list: MutableList<Birthday>): Boolean {
    var c: Int = 0
    for (x in list) {
        if (x.day == b.day) {
            c = c + 1
        }
    }
    return c == 1
}

fun monthWithUniqueDay(b: Birthday, list: MutableList<Birthday>): Boolean {
    for (x in list) {
        if ((x.month == b.month) && dayUnique(x, list)) {
            return true
        }
    }
    return false
}

fun bstr(b: Birthday): String {
    var months: MutableList<String> = mutableListOf("", "January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")
    return (months[b.month]!! + " ") + b.day.toString()
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        for (bd in choices) {
            if (!monthUnique(bd, choices)) {
                filtered = run { val _tmp = filtered.toMutableList(); _tmp.add(bd); _tmp } as MutableList<Birthday>
            }
        }
        for (bd in filtered) {
            if (!monthWithUniqueDay(bd, filtered)) {
                filtered2 = run { val _tmp = filtered2.toMutableList(); _tmp.add(bd); _tmp } as MutableList<Birthday>
            }
        }
        for (bd in filtered2) {
            if ((dayUnique(bd, filtered2)) as Boolean) {
                filtered3 = run { val _tmp = filtered3.toMutableList(); _tmp.add(bd); _tmp } as MutableList<Birthday>
            }
        }
        for (bd in filtered3) {
            if ((monthUnique(bd, filtered3)) as Boolean) {
                filtered4 = run { val _tmp = filtered4.toMutableList(); _tmp.add(bd); _tmp } as MutableList<Birthday>
            }
        }
        if (filtered4.size == 1) {
            println("Cheryl's birthday is " + bstr(filtered4[0]!!))
        } else {
            println("Something went wrong!")
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
