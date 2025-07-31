import java.math.BigInteger

var PI: Double = 3.141592653589793
var TWO_PI: Double = 6.283185307179586
fun sinApprox(x: Double): Double {
    var term: Double = x
    var sum: Double = x
    var n: Int = 1
    while (n <= 8) {
        var denom: Double = ((2 * n) * ((2 * n) + 1)).toDouble()
        term = (((0.0 - term) * x) * x) / denom
        sum = sum + term
        n = n + 1
    }
    return sum
}

fun floor(x: Double): Double {
    var i: Int = x.toInt()
    if (i.toDouble() > x) {
        i = i - 1
    }
    return i.toDouble()
}

fun absFloat(x: Double): Double {
    if (x < 0.0) {
        return 0.0 - x
    }
    return x
}

fun absInt(n: Int): Int {
    if (n < 0) {
        return 0 - n
    }
    return n
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
        n = (n * 10) + (digits)[str.substring(i, i + 1)] as Int
        i = i + 1
    }
    if (neg as Boolean) {
        n = 0 - n
    }
    return n
}

fun parseDate(s: String): MutableList<Int> {
    var y: Int = parseIntStr(s.substring(0, 4))
    var m: Int = parseIntStr(s.substring(5, 7))
    var d: Int = parseIntStr(s.substring(8, 10))
    return mutableListOf<Int>(y.toInt(), m.toInt(), d.toInt())
}

fun leap(y: Int): Boolean {
    if ((Math.floorMod(y, 400)) == 0) {
        return true
    }
    if ((Math.floorMod(y, 100)) == 0) {
        return false
    }
    return (Math.floorMod(y, 4)) == 0
}

fun daysInMonth(y: Int, m: Int): Int {
    var feb: Int = if (leap(y) != null) 29 else 28
    var lengths: MutableList<Int> = mutableListOf(31, feb, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
    return lengths[m - 1]!!
}

fun addDays(y: Int, m: Int, d: Int, n: Int): MutableList<Int> {
    var yy: Int = y
    var mm: Int = m
    var dd: Int = d
    if (n >= 0) {
        var i: Int = 0
        while (i < n) {
            dd = dd + 1
            if (dd > daysInMonth(yy, mm)) {
                dd = 1
                mm = mm + 1
                if (mm > 12) {
                    mm = 1
                    yy = yy + 1
                }
            }
            i = i + 1
        }
    } else {
        var i: Int = 0
        while (i > n) {
            dd = dd - 1
            if (dd < 1) {
                mm = mm - 1
                if (mm < 1) {
                    mm = 12
                    yy = yy - 1
                }
                dd = daysInMonth(yy, mm)
            }
            i = i - 1
        }
    }
    return mutableListOf(yy, mm, dd)
}

fun pad2(n: Int): String {
    if (n < 10) {
        return "0" + n.toString()
    }
    return n.toString()
}

fun dateString(y: Int, m: Int, d: Int): String {
    return (((y.toString() + "-") + pad2(m)) + "-") + pad2(d)
}

fun day(y: Int, m: Int, d: Int): Int {
    var part1: BigInteger = (367 * y).toBigInteger()
    var part2: Int = ((7 * (y + ((m + 9) / 12)).toInt()) / 4).toInt()
    var part3: Int = ((275 * m) / 9).toInt()
    return ((((part1.subtract(part2.toBigInteger())).add(part3.toBigInteger())).add(d.toBigInteger())).subtract(730530.toBigInteger())).toInt()
}

fun biorhythms(birth: String, target: String): Unit {
    var bparts: MutableList<Int> = parseDate(birth)
    var by: Int = bparts[0]!!
    var bm: Int = bparts[1]!!
    var bd: Int = bparts[2]!!
    var tparts: MutableList<Int> = parseDate(target)
    var ty: Int = tparts[0]!!
    var tm: Int = tparts[1]!!
    var td: Int = tparts[2]!!
    var diff: Int = absInt(day(ty, tm, td) - day(by, bm, bd))
    println((("Born " + birth) + ", Target ") + target)
    println("Day " + diff.toString())
    var cycles: MutableList<String> = mutableListOf("Physical day ", "Emotional day", "Mental day   ")
    var lengths: MutableList<Int> = mutableListOf(23, 28, 33)
    var quadrants: MutableList<MutableList<String>> = mutableListOf(mutableListOf("up and rising", "peak"), mutableListOf("up but falling", "transition"), mutableListOf("down and falling", "valley"), mutableListOf("down but rising", "transition"))
    var i: Int = 0
    while (i < 3) {
        var length: Int = lengths[i]!!
        var cycle: String = cycles[i]!!
        var position: BigInteger = (Math.floorMod(diff, length)).toBigInteger()
        var quadrant: BigInteger = (position.multiply(4.toBigInteger())).divide(length.toBigInteger())
        var percent: Double = sinApprox(((2.0 * PI) * position.toDouble()) / length.toDouble())
        percent = floor(percent * 1000.0) / 10.0
        var description: String = ""
        if (percent > 95.0) {
            description = " peak"
        } else {
            if (percent < (0.0 - 95.0)) {
                description = " valley"
            } else {
                if (absFloat(percent) < 5.0) {
                    description = " critical transition"
                } else {
                    var daysToAdd: BigInteger = (((quadrant.add(1.toBigInteger())).multiply(length.toBigInteger())).divide(4.toBigInteger())).subtract(position)
                    var res: MutableList<Int> = addDays(ty, tm, td, daysToAdd.toInt())
                    var ny: Int = res[0]!!
                    var nm: Int = res[1]!!
                    var nd: Int = res[2]!!
                    var transition: String = dateString(ny, nm, nd)
                    var trend: String = quadrants[(quadrant).toInt()]!![0]!!
                    var next: String = quadrants[(quadrant).toInt()]!![1]!!
                    var pct: String = percent.toString()
                    if (!((pct.contains(".")) as Boolean)) {
                        pct = pct + ".0"
                    }
                    description = (((((((" " + pct) + "% (") + trend) + ", next ") + next) + " ") + transition) + ")"
                }
            }
        }
        var posStr: String = position.toString()
        if (position.compareTo(10.toBigInteger()) < 0) {
            posStr = " " + posStr
        }
        println(((cycle + posStr) + " : ") + description)
        i = i + 1
    }
    println("")
}

fun user_main(): Unit {
    var pairs: MutableList<MutableList<String>> = mutableListOf(mutableListOf("1943-03-09", "1972-07-11"), mutableListOf("1809-01-12", "1863-11-19"), mutableListOf("1809-02-12", "1863-11-19"))
    var idx: Int = 0
    while (idx < pairs.size) {
        var p: MutableList<String> = pairs[idx]!!
        biorhythms(p[0]!!, p[1]!!)
        idx = idx + 1
    }
}

fun main() {
    user_main()
}
