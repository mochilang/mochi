fun isPrime(n: Int): Boolean {
    if (n < 2) {
        return false
    }
    if ((n % 2) == 0) {
        return n == 2
    }
    if ((n % 3) == 0) {
        return n == 3
    }
    var d: Int = 5
    while ((d * d) <= n) {
        if ((n % d) == 0) {
            return false
        }
        d = d + 2
        if ((n % d) == 0) {
            return false
        }
        d = d + 4
    }
    return true
}

fun sumDigits(n: Int): Int {
    var s: Int = 0
    var x: Int = n
    while (x > 0) {
        s = s + (x % 10)
        x = x / 10.toInt() as Int
    }
    return s
}

fun pad(n: Int): String {
    if (n < 10) {
        return "  " + n.toString()
    }
    if (n < 100) {
        return " " + n.toString()
    }
    return n.toString()
}

fun user_main(): Unit {
    println("Additive primes less than 500:")
    var count: Int = 0
    var line: String = ""
    var lineCount: Int = 0
    var i: Int = 2
    while (i < 500) {
        if (isPrime(i) as Boolean && isPrime(sumDigits(i) as Int) as Boolean) {
            count = count + 1
            line = (line + pad(i) as String) + "  "
            lineCount = lineCount + 1
            if (lineCount == 10) {
                println(line.substring(0, line.length - 2))
                line = ""
                lineCount = 0
            }
        }
        if (i > 2) {
            i = i + 2
        } else {
            i = i + 1
        }
    }
    if (lineCount > 0) {
        println(line.substring(0, line.length - 2))
    }
    println(count.toString() + " additive primes found.")
}

fun main() {
    user_main()
}
