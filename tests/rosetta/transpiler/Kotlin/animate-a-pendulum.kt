val PI: Double = 3.141592653589793
val L: Double = 10.0
val G: Double = 9.81
val dt: Double = 0.2
val phi0: Double = PI / 4.0
val omega: Double = sqrtApprox(G / L)
var t: Double = 0.0
fun sinApprox(x: Double): Double {
    var term: Double = x
    var sum: Double = x
    var n: Int = 1
    while (n <= 10) {
        val denom: Double = (2 * n) * ((2 * n) + 1).toDouble()
        term = (((0.0 - term) * x) * x) / denom
        sum = sum + term
        n = n + 1
    }
    return sum
}

fun cosApprox(x: Double): Double {
    var term: Double = 1.0
    var sum: Double = 1.0
    var n: Int = 1
    while (n <= 10) {
        val denom: Double = ((2 * n) - 1) * (2 * n).toDouble()
        term = (((0.0 - term) * x) * x) / denom
        sum = sum + term
        n = n + 1
    }
    return sum
}

fun sqrtApprox(x: Double): Double {
    var guess: Double = x
    var i: Int = 0
    while (i < 10) {
        guess = (guess + (x / guess)) / 2.0
        i = i + 1
    }
    return guess
}

fun main() {
    for (step in 0 until 10) {
        val phi: Double = phi0 * cosApprox(omega * t)
        val pos: Int = (10.0 * sinApprox(phi)) + 0.5.toInt()
        println(pos.toString())
        t = t + dt
    }
}
