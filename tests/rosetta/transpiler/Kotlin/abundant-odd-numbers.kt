fun divisors(n: Int): MutableList<Int> {
    var divs: MutableList<Int> = mutableListOf(1)
    var divs2: MutableList<Int> = mutableListOf()
    var i: Int = 2
    while ((i * i) <= n) {
        if ((n % i) == 0) {
            val j: Int = n / i.toInt()
            divs = run { val _tmp = divs.toMutableList(); _tmp.add(i); _tmp } as MutableList<Int>
            if (i != j) {
                divs2 = run { val _tmp = divs2.toMutableList(); _tmp.add(j); _tmp } as MutableList<Int>
            }
        }
        i = i + 1
    }
    var j: Int = divs2.size - 1
    while (j >= 0) {
        divs = run { val _tmp = divs.toMutableList(); _tmp.add(divs2[j]); _tmp } as MutableList<Int>
        j = j - 1
    }
    return divs as MutableList<Int>
}

fun sum(xs: MutableList<Int>): Int {
    var tot: Int = 0
    for (v in xs) {
        tot = tot + v
    }
    return tot as Int
}

fun sumStr(xs: MutableList<Int>): String {
    var s: String = ""
    var i: Int = 0
    while (i < xs.size) {
        s = (s + xs[i].toString()) + " + "
        i = i + 1
    }
    return s.substring(0, s.length - 3) as String
}

fun pad2(n: Int): String {
    val s: String = n.toString()
    if (s.length < 2) {
        return " " + s as String
    }
    return s as String
}

fun pad5(n: Int): String {
    var s: String = n.toString()
    while (s.length < 5) {
        s = " " + s
    }
    return s as String
}

fun abundantOdd(searchFrom: Int, countFrom: Int, countTo: Int, printOne: Boolean): Int {
    var count: Int = countFrom
    var n: Int = searchFrom
    while (count < countTo) {
        val divs: MutableList<Int> = divisors(n) as MutableList<Int>
        val tot: Int = divs.sum()
        if (tot > n) {
            count = count + 1
            if (printOne && (count < countTo)) {
                n = n + 2
                continue
            }
            val s: String = sumStr(divs) as String
            if (!printOne) {
                println((((((pad2(count) as String + ". ") + pad5(n) as String) + " < ") + s) + " = ") + tot.toString())
            } else {
                println((((n.toString() + " < ") + s) + " = ") + tot.toString())
            }
        }
        n = n + 2
    }
    return n as Int
}

fun user_main(): Unit {
    val max: Int = 25
    println(("The first " + max.toString()) + " abundant odd numbers are:")
    val n: Int = abundantOdd(1, 0, max, false) as Int
    println("\nThe one thousandth abundant odd number is:")
    abundantOdd(n, max, 1000, true) as Int
    println("\nThe first abundant odd number above one billion is:")
    abundantOdd(1000000001, 0, 1, true) as Int
}

fun main() {
    user_main()
}
