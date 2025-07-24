var testCases: MutableList<MutableList<Double>> = mutableListOf(mutableListOf(20.0, 45.0), mutableListOf(0 - 45.0, 45.0), mutableListOf(0 - 85.0, 90.0), mutableListOf(0 - 95.0, 90.0), mutableListOf(0 - 45.0, 125.0), mutableListOf(0 - 45.0, 145.0), mutableListOf(29.4803, 0 - 88.6381), mutableListOf(0 - 78.3251, 0 - 159.036), mutableListOf(0 - 70099.74233810938, 29840.67437876723), mutableListOf(0 - 165313.6666297357, 33693.9894517456), mutableListOf(1174.8380510598456, 0 - 154146.66490124757), mutableListOf(60175.77306795546, 42213.07192354373))
fun angleDiff(b1: Double, b2: Double): Double {
    val diff: Double = b2 - b1
    return ((((diff % 360.0) + 360.0) + 180.0) % 360.0) - 180.0
}

fun main() {
    for (tc in testCases) {
        println(angleDiff(tc[0], tc[1]))
    }
}
