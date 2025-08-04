import java.math.BigInteger

var months: MutableMap<String, Int> = mutableMapOf<String, Int>("January" to (1), "February" to (2), "March" to (3), "April" to (4), "May" to (5), "June" to (6), "July" to (7), "August" to (8), "September" to (9), "October" to (10), "November" to (11), "December" to (12))
fun isLeap(y: Int): Boolean {
    if ((Math.floorMod(y, 400)) == 0) {
        return true
    }
    if ((Math.floorMod(y, 100)) == 0) {
        return false
    }
    return (Math.floorMod(y, 4)) == 0
}

fun daysInMonth(y: Int, m: Int): Int {
    var feb: Int = if (isLeap(y) != null) 29 else 28
    var lengths: MutableList<Int> = mutableListOf(31, feb, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
    return lengths[m - 1]!!
}

fun daysBeforeYear(y: Int): Int {
    var days: Int = 0
    var yy: Int = 1970
    while (yy < y) {
        days = days + 365
        if (((isLeap(yy)) as Boolean)) {
            days = days + 1
        }
        yy = yy + 1
    }
    return days
}

fun daysBeforeMonth(y: Int, m: Int): Int {
    var days: Int = 0
    var mm: Int = 1
    while (mm < m) {
        days = days + daysInMonth(y, mm)
        mm = mm + 1
    }
    return days
}

fun epochSeconds(y: Int, m: Int, d: Int, h: Int, mi: Int): Int {
    var days: BigInteger = ((daysBeforeYear(y) + daysBeforeMonth(y, m)) + (d - 1)).toBigInteger()
    return ((((days.multiply((86400).toBigInteger())).add((h * 3600).toBigInteger())).add((mi * 60).toBigInteger())).toInt())
}

fun fromEpoch(sec: Int): MutableList<Int> {
    var days: Int = sec / 86400
    var rem: BigInteger = (Math.floorMod(sec, 86400)).toBigInteger()
    var y: Int = 1970
    while (true) {
        var dy: Int = if (isLeap(y) != null) 366 else 365
        if (days >= dy) {
            days = days - dy
            y = y + 1
        } else {
            break
        }
    }
    var m: Int = 1
    while (true) {
        var dim: Int = daysInMonth(y, m)
        if (days >= dim) {
            days = days - dim
            m = m + 1
        } else {
            break
        }
    }
    var d: BigInteger = (days + 1).toBigInteger()
    var h: BigInteger = rem.divide((3600).toBigInteger())
    var mi: BigInteger = (rem.remainder((3600).toBigInteger())).divide((60).toBigInteger())
    return (mutableListOf<Any?>((y as Any?), (m as Any?), (d as Any?), (h as Any?), (mi as Any?)) as MutableList<Int>)
}

fun pad2(n: Int): String {
    if (n < 10) {
        return "0" + n.toString()
    }
    return n.toString()
}

fun absInt(n: Int): Int {
    if (n < 0) {
        return 0 - n
    }
    return n
}

fun formatDate(parts: MutableList<Int>, offset: Int, abbr: String): String {
    var y: Int = parts[0]!!
    var m: Int = parts[1]!!
    var d: Int = parts[2]!!
    var h: Int = parts[3]!!
    var mi: Int = parts[4]!!
    var sign: String = "+"
    if (offset < 0) {
        sign = "-"
    }
    var off: Int = absInt(offset) / 60
    var offh: String = pad2(off / 60)
    var offm: String = pad2(Math.floorMod(off, 60))
    return (((((((((((((y.toString() + "-") + pad2(m)) + "-") + pad2(d)) + " ") + pad2(h)) + ":") + pad2(mi)) + ":00 ") + sign) + offh) + offm) + " ") + abbr
}

fun parseIntStr(str: String): Int {
    var i: Int = 0
    var neg: Boolean = false
    if ((str.length > 0) && (str.substring(0, 1) == "-")) {
        neg = true
        i = 1
    }
    var n: Int = 0
    var digits: MutableMap<String, Int> = mutableMapOf<String, Int>("0" to (0), "1" to (1), "2" to (2), "3" to (3), "4" to (4), "5" to (5), "6" to (6), "7" to (7), "8" to (8), "9" to (9))
    while (i < str.length) {
        n = (n * 10) + (digits)[str.substring(i, i + 1)]!!
        i = i + 1
    }
    if ((neg as Boolean)) {
        n = 0 - n
    }
    return n
}

fun indexOf(s: String, ch: String): Int {
    var i: Int = 0
    while (i < s.length) {
        if (s.substring(i, i + 1) == ch) {
            return i
        }
        i = i + 1
    }
    return 0 - 1
}

fun parseTime(s: String): MutableList<Int> {
    var c: Int = s.indexOf(":")
    var h: Int = Integer.parseInt(s.substring(0, c), 10)
    var mi: Int = Integer.parseInt(s.substring(c + 1, c + 3), 10)
    var ampm: String = s.substring(s.length - 2, s.length)
    var hh: (Int) -> Int = h
    if ((ampm == "pm") && (h != 12)) {
        hh = h + 12
    }
    if ((ampm == "am") && (h == 12)) {
        hh = 0
    }
    return mutableListOf<Int>((hh.toInt()), (mi.toInt()))
}

fun user_main(): Unit {
    var input: String = "March 7 2009 7:30pm EST"
    println("Input:              " + input)
    var parts = mutableListOf<Any?>()
    var cur: String = ""
    var i: Int = 0
    while (i < input.length) {
        var ch: String = input.substring(i, i + 1)
        if (ch == " ") {
            if (cur.length > 0) {
                parts = run { val _tmp = parts.toMutableList(); _tmp.add((cur as Any?)); _tmp }
                cur = ""
            }
        } else {
            cur = cur + ch
        }
        i = i + 1
    }
    if (cur.length > 0) {
        parts = run { val _tmp = parts.toMutableList(); _tmp.add((cur as Any?)); _tmp }
    }
    var month: Int = (months)[parts[0] as Any?]!!
    var day: Int = Integer.parseInt(parts[1] as Any?, 10)
    var year: Int = Integer.parseInt(parts[2] as Any?, 10)
    var tm: MutableList<Int> = parseTime(((parts[3] as Any?) as String))
    var hour: Int = tm[0]!!
    var minute: Int = tm[1]!!
    var tz: Any? = parts[4] as Any?
    var zoneOffsets: MutableMap<String, Int> = mutableMapOf<String, Int>("EST" to (0 - 18000), "EDT" to (0 - 14400), "MST" to (0 - 25200))
    var local: Int = epochSeconds((year.toInt()), month, (day.toInt()), hour, minute)
    var utc: BigInteger = (local - (zoneOffsets)[tz]!!).toBigInteger()
    var utc12: BigInteger = utc.add((43200).toBigInteger())
    var startDST: Int = epochSeconds(2009, 3, 8, 7, 0)
    var offEast: Int = 0 - 18000
    if (utc12.compareTo((startDST).toBigInteger()) >= 0) {
        offEast = 0 - 14400
    }
    var eastParts: MutableList<Int> = fromEpoch(((utc12.add((offEast).toBigInteger())).toInt()))
    var eastAbbr: String = "EST"
    if (offEast == (0 - 14400)) {
        eastAbbr = "EDT"
    }
    println("+12 hrs:            " + formatDate(eastParts, offEast, eastAbbr))
    var offAZ: Int = 0 - 25200
    var azParts: MutableList<Int> = fromEpoch(((utc12.add((offAZ).toBigInteger())).toInt()))
    println("+12 hrs in Arizona: " + formatDate(azParts, offAZ, "MST"))
}

fun main() {
    user_main()
}
