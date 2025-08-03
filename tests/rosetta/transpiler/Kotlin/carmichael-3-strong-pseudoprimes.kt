import java.math.BigInteger

fun mod(n: Int, m: Int): Int {
    return Math.floorMod(((Math.floorMod(n, m)) + m), m)
}

fun isPrime(n: Int): Boolean {
    if (n < 2) {
        return false
    }
    if ((Math.floorMod(n, 2)) == 0) {
        return n == 2
    }
    if ((Math.floorMod(n, 3)) == 0) {
        return n == 3
    }
    var d: Int = 5
    while ((d * d) <= n) {
        if ((Math.floorMod(n, d)) == 0) {
            return false
        }
        d = d + 2
        if ((Math.floorMod(n, d)) == 0) {
            return false
        }
        d = d + 4
    }
    return true
}

fun pad(n: Int, width: Int): String {
    var s: String = n.toString()
    while (s.length < width) {
        s = " " + s
    }
    return s
}

fun carmichael(p1: Int): Unit {
    for (h3 in 2 until p1) {
        for (d in 1 until h3 + p1) {
            if (((Math.floorMod(((h3 + p1) * (p1 - 1)), d)) == 0) && (mod((0 - p1) * p1, h3) == (Math.floorMod(d, h3)))) {
                var p2: BigInteger = (1 + (((p1 - 1) * (h3 + p1)) / d)).toBigInteger()
                if (!isPrime((p2.toInt()))) {
                    continue
                }
                var p3: BigInteger = (1).toBigInteger().add((((p1).toBigInteger().multiply((p2))).divide((h3).toBigInteger())))
                if (!isPrime((p3.toInt()))) {
                    continue
                }
                if (((p2.multiply((p3))).remainder((p1 - 1).toBigInteger())).compareTo((1).toBigInteger()) != 0) {
                    continue
                }
                var c: BigInteger = ((p1).toBigInteger().multiply((p2))).multiply((p3))
                println((((((pad(p1, 2) + "   ") + pad((p2.toInt()), 4)) + "   ") + pad((p3.toInt()), 5)) + "     ") + c.toString())
            }
        }
    }
}

fun main() {
    println("The following are Carmichael munbers for p1 <= 61:\n")
    println("p1     p2      p3     product")
    println("==     ==      ==     =======")
    for (p1 in 2 until 62) {
        if (((isPrime(p1)) as Boolean)) {
            carmichael(p1)
        }
    }
}
