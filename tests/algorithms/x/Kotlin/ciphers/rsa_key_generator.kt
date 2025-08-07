import java.math.BigInteger

fun pow2(n: Int): Long {
var v = 1L
var i = 0
while (i < n) {
v *= 2
i++
}
return v
}

data class Keys(var public_key: MutableList<Int> = mutableListOf<Int>(), var private_key: MutableList<Int> = mutableListOf<Int>())
var seed: Int = 1
var keys: Keys = generate_key(8)
var pub: MutableList<Int> = keys.public_key
var priv: MutableList<Int> = keys.private_key
fun next_seed(x: Int): Int {
    return ((Math.floorMod((((x * 1103515245) + 12345).toLong()), 2147483648L)).toInt())
}

fun rand_range(min: Int, max: Int): Int {
    seed = next_seed(seed)
    return min + (Math.floorMod(seed, (max - min)))
}

fun gcd(a: Int, b: Int): Int {
    var x: Int = a
    var y: Int = b
    while (y != 0) {
        var temp: Int = Math.floorMod(x, y)
        x = y
        y = temp
    }
    return x
}

fun mod_inverse(e: Int, phi: Int): Int {
    var t: Int = 0
    var newt: Int = 1
    var r: Int = phi
    var newr: Int = e
    while (newr != 0) {
        var quotient: Int = r / newr
        var tmp: Int = newt
        newt = t - (quotient * newt)
        t = tmp
        var tmp_r: Int = newr
        newr = r - (quotient * newr)
        r = tmp_r
    }
    if (r > 1) {
        return 0
    }
    if (t < 0) {
        t = t + phi
    }
    return t
}

fun is_prime(n: Int): Boolean {
    if (n < 2) {
        return false
    }
    var i: Int = 2
    while ((i * i) <= n) {
        if ((Math.floorMod(n, i)) == 0) {
            return false
        }
        i = i + 1
    }
    return true
}

fun generate_prime(bits: Int): Int {
    var min: Int = ((pow2(bits - 1)).toInt())
    var max: Int = ((pow2(bits)).toInt())
    var p: Int = rand_range(min, max)
    if ((Math.floorMod(p, 2)) == 0) {
        p = p + 1
    }
    while (!is_prime(p)) {
        p = p + 2
        if (p >= max) {
            p = min + 1
        }
    }
    return p
}

fun generate_key(bits: Int): Keys {
    var p: Int = generate_prime(bits)
    var q: Int = generate_prime(bits)
    var n: Int = p * q
    var phi: Int = (p - 1) * (q - 1)
    var e: Int = rand_range(2, phi)
    while (gcd(e, phi) != 1) {
        e = e + 1
        if (e >= phi) {
            e = 2
        }
    }
    var d: Int = mod_inverse(e, phi)
    return Keys(public_key = mutableListOf(n, e), private_key = mutableListOf(n, d))
}

fun main() {
    println(((("Public key: (" + ((pub as MutableList<Any?>)[0]).toString()) + ", ") + ((pub as MutableList<Any?>)[1]).toString()) + ")")
    println(((("Private key: (" + ((priv as MutableList<Any?>)[0]).toString()) + ", ") + ((priv as MutableList<Any?>)[1]).toString()) + ")")
}
