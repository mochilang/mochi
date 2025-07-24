fun pfacSum(i: Int): Int {
    var sum: Int = 0
    var p: Int = 1
    while (p <= (i / 2)) {
        if ((i % p) == 0) {
            sum = sum + p
        }
        p = p + 1
    }
    return sum
}

fun pad(n: Int, width: Int): String {
    var s: String = n.toString()
    while (s.length < width) {
        s = " " + s
    }
    return s
}

fun user_main(): Unit {
    var sums: MutableList<Int> = mutableListOf()
    var i: Int = 0
    while (i < 20000) {
        sums = run { val _tmp = sums.toMutableList(); _tmp.add(0); _tmp } as MutableList<Int>
        i = i + 1
    }
    i = 1
    while (i < 20000) {
        sums[i] = pfacSum(i)
        i = i + 1
    }
    println("The amicable pairs below 20,000 are:")
    var n: Int = 2
    while (n < 19999) {
        val m: Int = sums[n]
        if ((((m > n) && (m < 20000) as Boolean)) && (n == sums[m])) {
            println((("  " + pad(n, 5)) + " and ") + pad(m, 5))
        }
        n = n + 1
    }
}

fun main() {
    user_main()
}
