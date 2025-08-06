import java.math.BigInteger

fun panic(msg: String): Nothing { throw RuntimeException(msg) }

fun mod_pow(base: Int, exp: Int, mod: Int): Int {
    var result: Int = 1
    var b: BigInteger = ((Math.floorMod(base, mod)).toBigInteger())
    var e: Int = exp
    while (e > 0) {
        if ((Math.floorMod(e, 2)) == 1) {
            result = ((((result).toBigInteger().multiply((b))).remainder((mod).toBigInteger())).toInt())
        }
        b = (b.multiply((b))).remainder((mod).toBigInteger())
        e = e / 2
    }
    return result
}

fun miller_rabin(n: Int, allow_probable: Boolean): Boolean {
    if (n == 2) {
        return true
    }
    if ((n < 2) || ((Math.floorMod(n, 2)) == 0)) {
        return false
    }
    if (n > 5) {
        var last: Int = Math.floorMod(n, 10)
        if (!(((((((last == 1) || (last == 3) as Boolean)) || (last == 7) as Boolean)) || (last == 9)) as Boolean)) {
            return false
        }
    }
    var limit: Long = 3825123056546413051L
    if ((n > limit) && (!allow_probable as Boolean)) {
        panic("Warning: upper bound of deterministic test is exceeded. Pass allow_probable=true to allow probabilistic test.")
    }
    var bounds: MutableList<Int> = mutableListOf<Any?>((2047 as Any?), (1373653 as Any?), (25326001 as Any?), (3215031751L as Any?), (2152302898747L as Any?), (3474749660383L as Any?), (341550071728321L as Any?), (limit as Any?))
    var primes: MutableList<Int> = mutableListOf(2, 3, 5, 7, 11, 13, 17, 19)
    var i: Int = 0
    var plist_len: Int = primes.size
    while (i < bounds.size) {
        if (n < bounds[i]!!) {
            plist_len = i + 1
            i = bounds.size
        } else {
            i = i + 1
        }
    }
    var d: BigInteger = ((n - 1).toBigInteger())
    var s: Int = 0
    while ((d.remainder((2).toBigInteger())).compareTo((0).toBigInteger()) == 0) {
        d = d.divide((2).toBigInteger())
        s = s + 1
    }
    var j: Int = 0
    while (j < plist_len) {
        var prime: Int = primes[j]!!
        var x: Int = mod_pow(prime, (d.toInt()), n)
        var pr: Boolean = false
        if ((x == 1) || (x == (n - 1))) {
            pr = true
        } else {
            var r: Int = 1
            while ((r < s) && (!pr as Boolean)) {
                x = Math.floorMod((x * x), n)
                if (x == (n - 1)) {
                    pr = true
                }
                r = r + 1
            }
        }
        if (!pr) {
            return false
        }
        j = j + 1
    }
    return true
}

fun main() {
    println(miller_rabin(561, false).toString())
    println(miller_rabin(563, false).toString())
    println(miller_rabin(838201, false).toString())
    println(miller_rabin(838207, false).toString())
    println(miller_rabin(17316001, false).toString())
    println(miller_rabin(17316017, false).toString())
    println(miller_rabin((3078386641L.toInt()), false).toString())
    println(miller_rabin((3078386653L.toInt()), false).toString())
    println(miller_rabin((1713045574801L.toInt()), false).toString())
    println(miller_rabin((1713045574819L.toInt()), false).toString())
    println(miller_rabin((2779799728307L.toInt()), false).toString())
    println(miller_rabin((2779799728327L.toInt()), false).toString())
    println(miller_rabin((113850023909441L.toInt()), false).toString())
    println(miller_rabin((113850023909527L.toInt()), false).toString())
    println(miller_rabin((1275041018848804351L.toInt()), false).toString())
    println(miller_rabin((1275041018848804391L.toInt()), false).toString())
}
