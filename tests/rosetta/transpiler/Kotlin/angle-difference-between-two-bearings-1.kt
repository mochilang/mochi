var testCases: MutableList<MutableList<Double>> = mutableListOf(mutableListOf(20.0, 45.0), mutableListOf(0 - 45.0, 45.0), mutableListOf(0 - 85.0, 90.0), mutableListOf(0 - 95.0, 90.0), mutableListOf(0 - 45.0, 125.0), mutableListOf(0 - 45.0, 145.0), mutableListOf(29.4803, 0 - 88.6381), mutableListOf(0 - 78.3251, 0 - 159.036))
fun angleDiff(b1: Double, b2: Double): Double {
    val d: Double = b2 - b1
    if (d < (0 - 180.0)) {
        return d + 360.0
    }
    if (d > 180.0) {
        return d - 360.0
    }
    return d
}

fun main() {
    for (tc in testCases) {
        println(angleDiff(tc[0], tc[1]))
    }
}
