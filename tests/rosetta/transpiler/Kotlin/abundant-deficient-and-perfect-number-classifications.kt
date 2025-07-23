fun pfacSum(i: Int): Int {
    var sum: Int = 0
    var p: Int = 1
    while (p <= (i / 2)) {
        if ((i % p) == 0) {
            sum = sum + p
        }
        p = p + 1
    }
    return sum as Int
}

fun user_main(): Unit {
    var d: Int = 0
    var a: Int = 0
    var pnum: Int = 0
    var i: Int = 1
    while (i <= 20000) {
        val j: Int = pfacSum(i) as Int
        if (j < i) {
            d = d + 1
        }
        if (j == i) {
            pnum = pnum + 1
        }
        if (j > i) {
            a = a + 1
        }
        i = i + 1
    }
    println(("There are " + d.toString()) + " deficient numbers between 1 and 20000")
    println(("There are " + a.toString()) + " abundant numbers  between 1 and 20000")
    println(("There are " + pnum.toString()) + " perfect numbers between 1 and 20000")
}

fun main() {
    user_main()
}
