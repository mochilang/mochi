fun exp_approx(x: Double): Double {
    var sum: Double = 1.0
    var term: Double = 1.0
    var i: Int = (1).toInt()
    while (i <= 10) {
        term = (term * x) / (i.toDouble())
        sum = sum + term
        i = i + 1
    }
    return sum
}

fun sigmoid(vector: MutableList<Double>): MutableList<Double> {
    var result: MutableList<Double> = mutableListOf<Double>()
    var i: Int = (0).toInt()
    while (i < vector.size) {
        var v: Double = vector[i]!!
        var s: Double = 1.0 / (1.0 + exp_approx(0.0 - v))
        result = run { val _tmp = result.toMutableList(); _tmp.add(s); _tmp }
        i = i + 1
    }
    return result
}

fun main() {
    println(sigmoid(mutableListOf(0.0 - 1.0, 1.0, 2.0)).toString())
    println(sigmoid(mutableListOf(0.0)).toString())
}
