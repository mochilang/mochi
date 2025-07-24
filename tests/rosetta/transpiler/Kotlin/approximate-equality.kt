fun abs(x: Double): Double {
    if (x < 0.0) {
        return 0.0 - x
    }
    return x
}

fun maxf(a: Double, b: Double): Double {
    if (a > b) {
        return a
    }
    return b
}

fun isClose(a: Double, b: Double): Boolean {
    val relTol: Double = 0.000000001
    val t = kotlin.math.abs(a - b)
    val u: Double = relTol * maxf(kotlin.math.abs(a) as Double, kotlin.math.abs(b) as Double)
    return (t as Number).toDouble() <= u
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

fun user_main(): Unit {
    val root2: Double = sqrtApprox(2.0)
    val pairs: MutableList<MutableList<Double>> = mutableListOf(mutableListOf(100000000000000.02, 100000000000000.02), mutableListOf(100.01, 100.011), mutableListOf(10000000000000.002 / 10000.0, 1000000000.0000001), mutableListOf(0.001, 0.0010000001), mutableListOf(0.000000000000000000000101, 0.0), mutableListOf(root2 * root2, 2.0), mutableListOf((0.0 - root2) * root2, 0.0 - 2.0), mutableListOf(100000000000000000.0, 100000000000000000.0), mutableListOf(3.141592653589793, 3.141592653589793))
    for (pair in pairs) {
        val a = pair[0]
        val b = pair[1]
        val s: String = if (isClose(a as Double, b as Double) != null) "≈" else "≉"
        println((((a.toString() + " ") + s) + " ") + b.toString())
    }
}

fun main() {
    user_main()
}
