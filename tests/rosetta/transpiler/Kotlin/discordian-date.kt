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

val dayNames: MutableList<String> = mutableListOf("Sweetmorn", "Boomtime", "Pungenday", "Prickle-Prickle", "Setting Orange")
val seasons: MutableList<String> = mutableListOf("Chaos", "Discord", "Confusion", "Bureaucracy", "The Aftermath")
val holydays: MutableList<MutableList<String>> = mutableListOf(mutableListOf("Mungday", "Chaoflux"), mutableListOf("Mojoday", "Discoflux"), mutableListOf("Syaday", "Confuflux"), mutableListOf("Zaraday", "Bureflux"), mutableListOf("Maladay", "Afflux"))
val daysBefore: MutableList<Int> = mutableListOf(0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334)
fun isLeap(y: Int): Boolean {
    if ((Math.floorMod(y, 400)) == 0) {
        return true
    }
    if ((Math.floorMod(y, 100)) == 0) {
        return false
    }
    return (Math.floorMod(y, 4)) == 0
}

fun dayOfYear(y: Int, m: Int, d: Int): Int {
    var doy: BigInteger = (daysBefore[m - 1] + d).toBigInteger()
    if ((m > 2) && isLeap(y)) {
        doy = doy.add(1.toBigInteger())
    }
    return doy.toInt()
}

fun ordinal(n: Int): String {
    var suff: String = "th"
    val mod100: BigInteger = (Math.floorMod(n, 100)).toBigInteger()
    if ((mod100.compareTo(11.toBigInteger()) < 0) || (mod100.compareTo(13.toBigInteger()) > 0)) {
        val r: BigInteger = (Math.floorMod(n, 10)).toBigInteger()
        if (r.compareTo(1.toBigInteger()) == 0) {
            suff = "st"
        } else {
            if (r.compareTo(2.toBigInteger()) == 0) {
                suff = "nd"
            } else {
                if (r.compareTo(3.toBigInteger()) == 0) {
                    suff = "rd"
                }
            }
        }
    }
    return n.toString() + suff
}

fun discordian(y: Int, m: Int, d: Int): String {
    if (((isLeap(y) && (m == 2) as Boolean)) && (d == 29)) {
        return "St. Tib's Day, YOLD " + (y + 1166).toString()
    }
    var doy: Int = dayOfYear(y, m, d)
    if (isLeap(y) && (doy > 60)) {
        doy = doy - 1
    }
    var idx: BigInteger = (doy - 1).toBigInteger()
    val season: BigInteger = idx.divide(73.toBigInteger())
    val day: BigInteger = idx.remainder(73.toBigInteger())
    var res: String = (((((dayNames[(idx.remainder(5.toBigInteger())).toInt()] + ", the ") + ordinal((day.add(1.toBigInteger())).toInt())) + " day of ") + seasons[(season).toInt()]) + " in the YOLD ") + (y + 1166).toString()
    if (day.compareTo(4.toBigInteger()) == 0) {
        res = ((res + ". Celebrate ") + holydays[(season).toInt()][0]) + "!"
    }
    if (day.compareTo(49.toBigInteger()) == 0) {
        res = ((res + ". Celebrate ") + holydays[(season).toInt()][1]) + "!"
    }
    return res
}

fun user_main(): Unit {
    val dates: MutableList<MutableList<Int>> = mutableListOf(mutableListOf(2010, 7, 22), mutableListOf(2012, 2, 28), mutableListOf(2012, 2, 29), mutableListOf(2012, 3, 1), mutableListOf(2012, 12, 31), mutableListOf(2013, 1, 1), mutableListOf(2100, 12, 31), mutableListOf(2015, 10, 19), mutableListOf(2010, 1, 5), mutableListOf(2011, 5, 3), mutableListOf(2000, 3, 13))
    var i: Int = 0
    while (i < dates.size) {
        val dt: MutableList<Int> = dates[i]
        println(discordian(dt[0], dt[1], dt[2]))
        i = i + 1
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
