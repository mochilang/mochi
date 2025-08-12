import java.math.BigInteger

fun panic(msg: String): Nothing { throw RuntimeException(msg) }

data class Coeffs(var x: Int = 0, var y: Int = 0)
fun abs_val(n: Int): Int {
    if (n < 0) {
        return 0 - n
    }
    return n
}

fun extended_euclidean_algorithm(a: Int, b: Int): Coeffs {
    if (abs_val(a) == 1) {
        return Coeffs(x = a, y = 0)
    }
    if (abs_val(b) == 1) {
        return Coeffs(x = 0, y = b)
    }
    var old_remainder: Int = (a).toInt()
    var remainder: Int = (b).toInt()
    var old_coeff_a: Int = (1).toInt()
    var coeff_a: Int = (0).toInt()
    var old_coeff_b: Int = (0).toInt()
    var coeff_b: Int = (1).toInt()
    while (remainder != 0) {
        var quotient: Int = (old_remainder / remainder).toInt()
        var temp_remainder: Int = (old_remainder - (quotient * remainder)).toInt()
        old_remainder = remainder
        remainder = temp_remainder
        var temp_a: Int = (old_coeff_a - (quotient * coeff_a)).toInt()
        old_coeff_a = coeff_a
        coeff_a = temp_a
        var temp_b: Int = (old_coeff_b - (quotient * coeff_b)).toInt()
        old_coeff_b = coeff_b
        coeff_b = temp_b
    }
    if (a < 0) {
        old_coeff_a = 0 - old_coeff_a
    }
    if (b < 0) {
        old_coeff_b = 0 - old_coeff_b
    }
    return Coeffs(x = old_coeff_a, y = old_coeff_b)
}

fun test_extended_euclidean_algorithm(): Unit {
    var r1: Coeffs = extended_euclidean_algorithm(1, 24)
    if ((r1.x != 1) || (r1.y != 0)) {
        panic("test1 failed")
    }
    var r2: Coeffs = extended_euclidean_algorithm(8, 14)
    if ((r2.x != 2) || (r2.y != (0 - 1))) {
        panic("test2 failed")
    }
    var r3: Coeffs = extended_euclidean_algorithm(240, 46)
    if ((r3.x != (0 - 9)) || (r3.y != 47)) {
        panic("test3 failed")
    }
    var r4: Coeffs = extended_euclidean_algorithm(1, 0 - 4)
    if ((r4.x != 1) || (r4.y != 0)) {
        panic("test4 failed")
    }
    var r5: Coeffs = extended_euclidean_algorithm(0 - 2, 0 - 4)
    if ((r5.x != (0 - 1)) || (r5.y != 0)) {
        panic("test5 failed")
    }
    var r6: Coeffs = extended_euclidean_algorithm(0, 0 - 4)
    if ((r6.x != 0) || (r6.y != (0 - 1))) {
        panic("test6 failed")
    }
    var r7: Coeffs = extended_euclidean_algorithm(2, 0)
    if ((r7.x != 1) || (r7.y != 0)) {
        panic("test7 failed")
    }
}

fun user_main(): Unit {
    test_extended_euclidean_algorithm()
    var res: Coeffs = extended_euclidean_algorithm(240, 46)
    println(((("(" + res.x.toString()) + ", ") + res.y.toString()) + ")")
}

fun main() {
    user_main()
}
