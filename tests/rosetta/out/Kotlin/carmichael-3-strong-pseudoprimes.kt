// Generated by Mochi compiler v0.10.28 on 2006-01-02T15:04:05Z
// Code generated from carmichael-3-strong-pseudoprimes.mochi

/**
 * Auto-generated from Mochi
 * @param n Int
 * @param m Int
 * @return Int
 */
fun mod(n: Int, m: Int): Int {
    return ((n % m) + m) % m
}

/**
 * Auto-generated from Mochi
 * @param n Int
 * @return Boolean
 */
fun isPrime(n: Int): Boolean {
    if (n < 2) {
        return false
    }
    if (n % 2 == 0) {
        return n == 2
    }
    if (n % 3 == 0) {
        return n == 3
    }
    var d = 5
    while (d * d <= n) {
        if (n % d == 0) {
            return false
        }
        d = d + 2
        if (n % d == 0) {
            return false
        }
        d = d + 4
    }
    return true
}

/**
 * Auto-generated from Mochi
 * @param n Int
 * @param width Int
 * @return String
 */
fun pad(n: Int, width: Int): String {
    var s = n.toString()
    while (s.length < width) {
        s = " " + s
    }
    return s
}

/**
 * Auto-generated from Mochi
 * @param p1 Int
 */
fun carmichael(p1: Int): Unit {
    for (h3 in 2 until p1) {
        for (d in 1 until (h3 + p1)) {
            if (((h3 + p1) * (p1 - 1)) % d == 0 && mod(-p1 * p1, h3) == d % h3) {
                val p2 = (1).toDouble() + (((p1 - 1) * (h3 + p1)).toDouble() / (d).toDouble())
                if (!isPrime(p2)) {
                    continue
                }
                val p3 = (1).toDouble() + ((p1).toDouble() * p2 / (h3).toDouble())
                if (!isPrime(p3)) {
                    continue
                }
                if ((p2 * p3) % ((p1 - 1)).toDouble() != 1) {
                    continue
                }
                val c = (p1).toDouble() * p2 * p3
                println(pad(p1, 2) + "   " + pad(p2, 4) + "   " + pad(p3, 5) + "     " + c.toString())
            }
        }
    }
}

fun main() {
    println("The following are Carmichael munbers for p1 <= 61:\n")
    println("p1     p2      p3     product")
    println("==     ==      ==     =======")
    for (p1 in 2 until 62) {
        if (isPrime(p1)) {
            carmichael(p1)
        }
    }
}
