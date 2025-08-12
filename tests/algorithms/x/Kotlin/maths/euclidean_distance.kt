fun sqrtApprox(x: Double): Double {
    if (x <= 0.0) {
        return 0.0
    }
    var guess: Double = x
    var i: Int = (0).toInt()
    while (i < 20) {
        guess = (guess + (x / guess)) / 2.0
        i = i + 1
    }
    return guess
}

fun euclidean_distance(v1: MutableList<Double>, v2: MutableList<Double>): Double {
    var sum: Double = 0.0
    var i: Int = (0).toInt()
    while (i < v1.size) {
        var diff: Double = v1[i]!! - v2[i]!!
        sum = sum + (diff * diff)
        i = i + 1
    }
    return sqrtApprox(sum)
}

fun euclidean_distance_no_np(v1: MutableList<Double>, v2: MutableList<Double>): Double {
    return euclidean_distance(v1, v2)
}

fun user_main(): Unit {
    println(euclidean_distance(mutableListOf(0.0, 0.0), mutableListOf(2.0, 2.0)).toString())
    println(euclidean_distance(mutableListOf(0.0, 0.0, 0.0), mutableListOf(2.0, 2.0, 2.0)).toString())
    println(euclidean_distance(mutableListOf(1.0, 2.0, 3.0, 4.0), mutableListOf(5.0, 6.0, 7.0, 8.0)).toString())
    println(euclidean_distance_no_np(mutableListOf(1.0, 2.0, 3.0, 4.0), mutableListOf(5.0, 6.0, 7.0, 8.0)).toString())
    println(euclidean_distance_no_np(mutableListOf(0.0, 0.0), mutableListOf(2.0, 2.0)).toString())
}

fun main() {
    user_main()
}
