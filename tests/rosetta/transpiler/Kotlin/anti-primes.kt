fun countDivisors(n: Int): Int {
    if (n < 2) {
        return 1
    }
    var count: Int = 2
    var i: Int = 2
    while (i <= (n / 2)) {
        if ((n % i) == 0) {
            count = count + 1
        }
        i = i + 1
    }
    return count
}

fun user_main(): Unit {
    println("The first 20 anti-primes are:")
    var maxDiv: Int = 0
    var count: Int = 0
    var n: Int = 1
    var line: String = ""
    while (count < 20) {
        val d: Int = countDivisors(n)
        if (d > maxDiv) {
            line = (line + n.toString()) + " "
            maxDiv = d
            count = count + 1
        }
        n = n + 1
    }
    line = line.substring(0, line.length - 1) as String
    println(line)
}

fun main() {
    user_main()
}
