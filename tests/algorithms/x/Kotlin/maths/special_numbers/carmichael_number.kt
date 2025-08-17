import java.math.BigInteger

fun panic(msg: String): Nothing { throw RuntimeException(msg) }

fun _numToStr(v: Number): String {
    val d = v.toDouble()
    val i = d.toLong()
    return if (d == i.toDouble()) i.toString() else d.toString()
}

fun abs_int(x: Int): Int {
    if (x < 0) {
        return 0 - x
    }
    return x
}

fun gcd(a: Int, b: Int): Int {
    if (a == 0) {
        return abs_int(b)
    }
    return gcd(Math.floorMod(b, a), a)
}

fun power(x: Int, y: Int, m: Int): Int {
    if (y == 0) {
        return Math.floorMod(1, m)
    }
    var temp: Int = (Math.floorMod(power(x, y / 2, m), m)).toInt()
    temp = Math.floorMod((temp * temp), m)
    if ((Math.floorMod(y, 2)) == 1) {
        temp = Math.floorMod((temp * x), m)
    }
    return temp
}

fun is_carmichael_number(n: Int): Boolean {
    if (n <= 0) {
        panic("Number must be positive")
    }
    var b: Int = (2).toInt()
    while (b < n) {
        if (gcd(b, n) == 1) {
            if (power(b, n - 1, n) != 1) {
                return false
            }
        }
        b = b + 1
    }
    return true
}

fun main() {
    println(_numToStr(power(2, 15, 3)))
    println(_numToStr(power(5, 1, 30)))
    println(is_carmichael_number(4).toString())
    println(is_carmichael_number(561).toString())
    println(is_carmichael_number(562).toString())
    println(is_carmichael_number(1105).toString())
}
