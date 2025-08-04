var dxs: MutableList<Double> = mutableListOf(0.0 - 0.533, 0.27, 0.859, 0.0 - 0.043, 0.0 - 0.205, 0.0 - 0.127, 0.0 - 0.071, 0.275, 1.251, 0.0 - 0.231, 0.0 - 0.401, 0.269, 0.491, 0.951, 1.15, 0.001, 0.0 - 0.382, 0.161, 0.915, 2.08, 0.0 - 2.337, 0.034, 0.0 - 0.126, 0.014, 0.709, 0.129, 0.0 - 1.093, 0.0 - 0.483, 0.0 - 1.193, 0.02, 0.0 - 0.051, 0.047, 0.0 - 0.095, 0.695, 0.34, 0.0 - 0.182, 0.287, 0.213, 0.0 - 0.423, 0.0 - 0.021, 0.0 - 0.134, 1.798, 0.021, 0.0 - 1.099, 0.0 - 0.361, 1.636, 0.0 - 1.134, 1.315, 0.201, 0.034, 0.097, 0.0 - 0.17, 0.054, 0.0 - 0.553, 0.0 - 0.024, 0.0 - 0.181, 0.0 - 0.7, 0.0 - 0.361, 0.0 - 0.789, 0.279, 0.0 - 0.174, 0.0 - 0.009, 0.0 - 0.323, 0.0 - 0.658, 0.348, 0.0 - 0.528, 0.881, 0.021, 0.0 - 0.853, 0.157, 0.648, 1.774, 0.0 - 1.043, 0.051, 0.021, 0.247, 0.0 - 0.31, 0.171, 0.0, 0.106, 0.024, 0.0 - 0.386, 0.962, 0.765, 0.0 - 0.125, 0.0 - 0.289, 0.521, 0.017, 0.281, 0.0 - 0.749, 0.0 - 0.149, 0.0 - 2.436, 0.0 - 0.909, 0.394, 0.0 - 0.113, 0.0 - 0.598, 0.443, 0.0 - 0.521, 0.0 - 0.799, 0.087)
var dys: MutableList<Double> = mutableListOf(0.136, 0.717, 0.459, 0.0 - 0.225, 1.392, 0.385, 0.121, 0.0 - 0.395, 0.49, 0.0 - 0.682, 0.0 - 0.065, 0.242, 0.0 - 0.288, 0.658, 0.459, 0.0, 0.426, 0.205, 0.0 - 0.765, 0.0 - 2.188, 0.0 - 0.742, 0.0 - 0.01, 0.089, 0.208, 0.585, 0.633, 0.0 - 0.444, 0.0 - 0.351, 0.0 - 1.087, 0.199, 0.701, 0.096, 0.0 - 0.025, 0.0 - 0.868, 1.051, 0.157, 0.216, 0.162, 0.249, 0.0 - 0.007, 0.009, 0.508, 0.0 - 0.79, 0.723, 0.881, 0.0 - 0.508, 0.393, 0.0 - 0.226, 0.71, 0.038, 0.0 - 0.217, 0.831, 0.48, 0.407, 0.447, 0.0 - 0.295, 1.126, 0.38, 0.549, 0.0 - 0.445, 0.0 - 0.046, 0.428, 0.0 - 0.074, 0.217, 0.0 - 0.822, 0.491, 1.347, 0.0 - 0.141, 1.23, 0.0 - 0.044, 0.079, 0.219, 0.698, 0.275, 0.056, 0.031, 0.421, 0.064, 0.721, 0.104, 0.0 - 0.729, 0.65, 0.0 - 1.103, 0.154, 0.0 - 1.72, 0.051, 0.0 - 0.385, 0.477, 1.537, 0.0 - 0.901, 0.939, 0.0 - 0.411, 0.341, 0.0 - 0.411, 0.106, 0.224, 0.0 - 0.947, 0.0 - 1.424, 0.0 - 0.542, 0.0 - 1.032)
fun sqrtApprox(x: Double): Double {
    if (x <= 0.0) {
        return 0.0
    }
    var g: Double = x
    var i: Int = 0
    while (i < 20) {
        g = (g + (x / g)) / 2.0
        i = i + 1
    }
    return g
}

fun funnel(fa: MutableList<Double>, r: (Double, Double) -> Double): MutableList<Double> {
    var x: Double = 0.0
    var result = mutableListOf<Any?>()
    var i: Int = 0
    while (i < fa.size) {
        var f: Double = fa[i]!!
        result = run { val _tmp = result.toMutableList(); _tmp.add(((x + f) as Any?)); _tmp }
        x = ((r(x, f)).toDouble())
        i = i + 1
    }
    return (result as MutableList<Double>)
}

fun mean(fa: MutableList<Double>): Double {
    var sum: Double = 0.0
    var i: Int = 0
    while (i < fa.size) {
        sum = sum + fa[i]!!
        i = i + 1
    }
    return sum / ((fa.size.toDouble()))
}

fun stdDev(fa: MutableList<Double>): Double {
    var m: Double = mean(fa)
    var sum: Double = 0.0
    var i: Int = 0
    while (i < fa.size) {
        var d: Double = fa[i]!! - m
        sum = sum + (d * d)
        i = i + 1
    }
    var r: Double = sqrtApprox(sum / ((fa.size.toDouble())))
    return r
}

fun experiment(label: String, r: (Double, Double) -> Double): Unit {
    var rxs: MutableList<Double> = funnel(dxs, r)
    var rys: MutableList<Double> = funnel(dys, r)
    println(label + "  :      x        y")
    println((("Mean    :  " + mean(rxs).toString()) + ", ") + mean(rys).toString())
    println((("Std Dev :  " + stdDev(rxs).toString()) + ", ") + stdDev(rys).toString())
    println("")
}

fun user_main(): Unit {
    experiment("Rule 1", ({ x: Double, y: Double -> 0.0 } as (Double, Double) -> Double))
    experiment("Rule 2", ({ x: Double, dz: Double -> 0.0 - dz } as (Double, Double) -> Double))
    experiment("Rule 3", ({ z: Double, dz: Double -> 0.0 - (z + dz) } as (Double, Double) -> Double))
    experiment("Rule 4", ({ z: Double, dz: Double -> z + dz } as (Double, Double) -> Double))
}

fun main() {
    user_main()
}
