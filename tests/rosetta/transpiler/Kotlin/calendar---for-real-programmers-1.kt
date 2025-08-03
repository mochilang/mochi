import java.math.BigInteger

var daysInMonth: MutableList<Int> = mutableListOf(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
var start: MutableList<Int> = mutableListOf(3, 6, 6, 2, 4, 0, 2, 5, 1, 3, 6, 1)
var months: MutableList<String> = mutableListOf(" January ", " February", "  March  ", "  April  ", "   May   ", "   June  ", "   July  ", "  August ", "September", " October ", " November", " December")
var days: MutableList<String> = mutableListOf("Su", "Mo", "Tu", "We", "Th", "Fr", "Sa")
fun main() {
    println("                                [SNOOPY]\n")
    println("                                  1969\n")
    var qtr: Int = 0
    while (qtr < 4) {
        var mi: Int = 0
        while (mi < 3) {
            println(listOf(("      " + months[(qtr * 3) + mi]!!) + "           ", false).joinToString(" "))
            mi = mi + 1
        }
        println("")
        mi = 0
        while (mi < 3) {
            var d: Int = 0
            while (d < 7) {
                println(listOf(" " + days[d]!!, false).joinToString(" "))
                d = d + 1
            }
            println(listOf("     ", false).joinToString(" "))
            mi = mi + 1
        }
        println("")
        var week: Int = 0
        while (week < 6) {
            mi = 0
            while (mi < 3) {
                var day: Int = 0
                while (day < 7) {
                    var m: BigInteger = ((qtr * 3) + mi).toBigInteger()
                    var _val: BigInteger = ((((week * 7) + day) - start[(m).toInt()]!!) + 1).toBigInteger()
                    if ((_val.compareTo((1).toBigInteger()) >= 0) && (_val.compareTo((daysInMonth[(m).toInt()]!!).toBigInteger()) <= 0)) {
                        var s: String = _val.toString()
                        if (s.length == 1) {
                            s = " " + s
                        }
                        println(listOf(" " + s, false).joinToString(" "))
                    } else {
                        println(listOf("   ", false).joinToString(" "))
                    }
                    day = day + 1
                }
                println(listOf("     ", false).joinToString(" "))
                mi = mi + 1
            }
            println("")
            week = week + 1
        }
        println("")
        qtr = qtr + 1
    }
}
