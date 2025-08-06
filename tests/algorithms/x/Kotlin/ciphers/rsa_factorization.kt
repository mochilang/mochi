import java.math.BigInteger

fun gcd(a: Int, b: Int): Int {
    var x: Int = a
    var y: Int = b
    while (y != 0) {
        var t: Int = Math.floorMod(x, y)
        x = y
        y = t
    }
    if (x < 0) {
        return 0 - x
    }
    return x
}

fun pow_mod(base: Int, exp: Int, mod: Int): Int {
    var result: Int = 1
    var b: BigInteger = ((Math.floorMod(base, mod)).toBigInteger())
    var e: Int = exp
    while (e > 0) {
        if ((Math.floorMod(e, 2)) == 1) {
            result = ((((result).toBigInteger().multiply((b))).remainder((mod).toBigInteger())).toInt())
        }
        e = e / 2
        b = (b.multiply((b))).remainder((mod).toBigInteger())
    }
    return result
}

fun rsa_factor(d: Int, e: Int, n: Int): MutableList<Int> {
    var k: Int = (d * e) - 1
    var p: Int = 0
    var q: Int = 0
    var g: Int = 2
    while ((p == 0) && (g < n)) {
        var t: Int = k
        while ((Math.floorMod(t, 2)) == 0) {
            t = t / 2
            var x: Int = pow_mod(g, t, n)
            var y: Int = gcd(x - 1, n)
            if ((x > 1) && (y > 1)) {
                p = y
                q = n / y
                break
            }
        }
        g = g + 1
    }
    if (p > q) {
        return mutableListOf(q, p)
    }
    return mutableListOf(p, q)
}

fun main() {
    println(rsa_factor(3, 16971, 25777))
    println(rsa_factor(7331, 11, 27233))
    println(rsa_factor(4021, 13, 17711))
}
