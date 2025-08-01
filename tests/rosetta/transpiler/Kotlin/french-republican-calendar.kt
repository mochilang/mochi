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

var gregorianStr: MutableList<String> = mutableListOf("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")
var gregorian: MutableList<Int> = mutableListOf(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
var republicanStr: MutableList<String> = mutableListOf("Vendemiaire", "Brumaire", "Frimaire", "Nivose", "Pluviose", "Ventose", "Germinal", "Floreal", "Prairial", "Messidor", "Thermidor", "Fructidor")
var sansculotidesStr: MutableList<String> = mutableListOf("Fete de la vertu", "Fete du genie", "Fete du travail", "Fete de l'opinion", "Fete des recompenses", "Fete de la Revolution")
var rep: MutableList<Int> = dayToRep(greToDay(20, 5, 1795))
fun greLeap(year: Int): Boolean {
    var a: Int = (Math.floorMod(year, 4)).toInt()
    var b: Int = (Math.floorMod(year, 100)).toInt()
    var c: Int = (Math.floorMod(year, 400)).toInt()
    return ((a == 0) && (((b != 0) || (c == 0) as Boolean))) as Boolean
}

fun repLeap(year: Int): Boolean {
    var a: Int = (Math.floorMod((year + 1), 4)).toInt()
    var b: Int = (Math.floorMod((year + 1), 100)).toInt()
    var c: Int = (Math.floorMod((year + 1), 400)).toInt()
    return ((a == 0) && (((b != 0) || (c == 0) as Boolean))) as Boolean
}

fun greToDay(d: Int, m: Int, y: Int): Int {
    var yy: Int = y
    var mm: Int = m
    if (mm < 3) {
        yy = yy - 1
        mm = mm + 12
    }
    return ((((((yy * 36525) / 100) - (yy / 100)) + (yy / 400)) + ((306 * (mm + 1)) / 10)) + d) - 654842
}

fun repToDay(d: Int, m: Int, y: Int): Int {
    var dd: Int = d
    var mm: Int = m
    if (mm == 13) {
        mm = mm - 1
        dd = dd + 30
    }
    if ((repLeap(y)) as Boolean) {
        dd = dd - 1
    }
    return ((((((365 * y) + ((y + 1) / 4)) - ((y + 1) / 100)) + ((y + 1) / 400)) + (30 * mm)) + dd) - 395
}

fun dayToGre(day: Int): MutableList<Int> {
    var y: BigInteger = ((day * 100) / 36525).toBigInteger()
    var d: BigInteger = ((day).toBigInteger().subtract(((y.multiply(36525.toBigInteger())).divide(100.toBigInteger())))).add(21.toBigInteger())
    y = y.add(1792.toBigInteger())
    d = ((d.add((y.divide(100.toBigInteger())))).subtract((y.divide(400.toBigInteger())))).subtract(13.toBigInteger())
    var m: Int = 8
    while (d.compareTo(gregorian[m]!!.toBigInteger()) > 0) {
        d = d.subtract(gregorian[m]!!.toBigInteger())
        m = m + 1
        if (m == 12) {
            m = 0
            y = y.add(1.toBigInteger())
            if ((greLeap(y.toInt())) as Boolean) {
                (gregorian[1]) = 29
            } else {
                (gregorian[1]) = 28
            }
        }
    }
    m = m + 1
    return mutableListOf<Int>(d.toInt(), m, y.toInt())
}

fun dayToRep(day: Int): MutableList<Int> {
    var y: BigInteger = (((day - 1) * 100) / 36525).toBigInteger()
    if ((repLeap(y.toInt())) as Boolean) {
        y = y.subtract(1.toBigInteger())
    }
    var d: BigInteger = ((((day).toBigInteger().subtract((((y.add(1.toBigInteger())).multiply(36525.toBigInteger())).divide(100.toBigInteger())))).add(365.toBigInteger())).add(((y.add(1.toBigInteger())).divide(100.toBigInteger())))).subtract(((y.add(1.toBigInteger())).divide(400.toBigInteger())))
    y = y.add(1.toBigInteger())
    var m: Int = 1
    var sc: Int = 5
    if ((repLeap(y.toInt())) as Boolean) {
        sc = 6
    }
    while (d.compareTo(30.toBigInteger()) > 0) {
        d = d.subtract(30.toBigInteger())
        m = m + 1
        if (m == 13) {
            if (d.compareTo(sc.toBigInteger()) > 0) {
                d = d.subtract(sc.toBigInteger())
                m = 1
                y = y.add(1.toBigInteger())
                sc = 5
                if ((repLeap(y.toInt())) as Boolean) {
                    sc = 6
                }
            }
        }
    }
    return mutableListOf<Int>(d.toInt(), m, y.toInt())
}

fun formatRep(d: Int, m: Int, y: Int): String {
    if (m == 13) {
        return (sansculotidesStr[d - 1]!! + " ") + y.toString()
    }
    return (((d.toString() + " ") + republicanStr[m - 1]!!) + " ") + y.toString()
}

fun formatGre(d: Int, m: Int, y: Int): String {
    return (((d.toString() + " ") + gregorianStr[m - 1]!!) + " ") + y.toString()
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println(formatRep(rep[0]!!, rep[1]!!, rep[2]!!))
        var gre: MutableList<Int> = dayToGre(repToDay(1, 9, 3))
        println(formatGre(gre[0]!!, gre[1]!!, gre[2]!!))
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
