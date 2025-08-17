import java.math.BigInteger

fun panic(msg: String): Nothing { throw RuntimeException(msg) }

fun pow_float(base: Double, exponent: Int): Double {
    var exp: Int = (exponent).toInt()
    var result: Double = 1.0
    var i: Int = (0).toInt()
    while (i < exp) {
        result = result * base
        i = i + 1
    }
    return result
}

fun evaluate_poly(poly: MutableList<Double>, x: Double): Double {
    var total: Double = 0.0
    var i: Int = (0).toInt()
    while (i < poly.size) {
        total = total + (poly[i]!! * pow_float(x, i))
        i = i + 1
    }
    return total
}

fun horner(poly: MutableList<Double>, x: Double): Double {
    var result: Double = 0.0
    var i: Int = (poly.size - 1).toInt()
    while (i >= 0) {
        result = (result * x) + poly[i]!!
        i = i - 1
    }
    return result
}

fun test_polynomial_evaluation(): Unit {
    var poly: MutableList<Double> = mutableListOf(0.0, 0.0, 5.0, 9.3, 7.0)
    var x: Double = 10.0
    if (evaluate_poly(poly, x) != 79800.0) {
        panic("evaluate_poly failed")
    }
    if (horner(poly, x) != 79800.0) {
        panic("horner failed")
    }
}

fun user_main(): Unit {
    test_polynomial_evaluation()
    var poly: MutableList<Double> = mutableListOf(0.0, 0.0, 5.0, 9.3, 7.0)
    var x: Double = 10.0
    println(evaluate_poly(poly, x))
    println(horner(poly, x))
}

fun main() {
    user_main()
}
