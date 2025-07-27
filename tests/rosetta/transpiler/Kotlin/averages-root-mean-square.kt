val n: Int = 10
var sum: Double = 0.0
var x: Int = 1
val rms: Double = sqrtApprox(sum / n.toDouble())
fun sqrtApprox(x: Double): Double {
    var guess: Double = x
    var i: Int = 0
    while (i < 20) {
        guess = (guess + (x / guess)) / 2.0
        i = i + 1
    }
    return guess
}

fun main() {
    while (x <= n) {
        sum = sum + (x.toDouble() * x.toDouble())
        x = x + 1
    }
    println(rms.toString())
}
