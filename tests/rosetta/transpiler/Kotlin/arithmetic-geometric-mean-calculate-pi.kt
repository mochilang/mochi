fun abs(x: Double): Double {
    if (x < 0.0) {
        return 0.0 - x
    }
    return x
}

fun sqrtApprox(x: Double): Double {
    var guess: Double = x
    var i: Int = 0
    while (i < 20) {
        guess = (guess + (x / guess)) / 2.0
        i = i + 1
    }
    return guess
}

fun agmPi(): Double {
    var a: Double = 1.0
    var g: Double = 1.0 / sqrtApprox(2.0)
    var sum: Double = 0.0
    var pow: Double = 2.0
    while (kotlin.math.abs(a - g) > 0.000000000000001) {
        var t: Double = (a + g) / 2.0
        var u: Double = sqrtApprox(a * g)
        a = t
        g = u
        pow = pow * 2.0
        var diff: Double = (a * a) - (g * g)
        sum = sum + (diff * pow)
    }
    var pi: Double = ((4.0 * a) * a) / (1.0 - sum)
    return pi
}

fun user_main(): Unit {
    println(agmPi().toString())
}

fun main() {
    user_main()
}
