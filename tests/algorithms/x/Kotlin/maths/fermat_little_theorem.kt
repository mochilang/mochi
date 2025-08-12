import java.math.BigInteger

var p: Int = (701).toInt()
var a: Int = (1000000000).toInt()
var b: Int = (10).toInt()
var left: Int = (Math.floorMod((a / b), p)).toInt()
var right_fast: Int = (Math.floorMod((a * binary_exponentiation(b, p - 2, p)), p)).toInt()
fun binary_exponentiation(a: Int, n: Int, mod: Int): Int {
    if (n == 0) {
        return 1
    }
    if ((Math.floorMod(n, 2)) == 1) {
        return Math.floorMod((binary_exponentiation(a, n - 1, mod) * a), mod)
    }
    var b: Int = (binary_exponentiation(a, n / 2, mod)).toInt()
    return Math.floorMod((b * b), mod)
}

fun naive_exponent_mod(a: Int, n: Int, mod: Int): Int {
    var result: Int = (1).toInt()
    var i: Int = (0).toInt()
    while (i < n) {
        result = Math.floorMod((result * a), mod)
        i = i + 1
    }
    return result
}

fun print_bool(b: Boolean): Unit {
    if ((b as Boolean)) {
        println(true)
    } else {
        println(false)
    }
}

fun main() {
    print_bool(left.compareTo((right_fast)) == 0)
    var right_naive: Int = (Math.floorMod((a * naive_exponent_mod(b, p - 2, p)), p)).toInt()
    print_bool(left.compareTo((right_naive)) == 0)
}
