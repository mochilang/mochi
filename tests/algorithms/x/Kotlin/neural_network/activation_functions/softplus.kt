fun panic(msg: String): Nothing { throw RuntimeException(msg) }

fun ln(x: Double): Double {
    if (x <= 0.0) {
        panic("ln domain error")
    }
    var y: Double = (x - 1.0) / (x + 1.0)
    var y2: Double = y * y
    var term: Double = y
    var sum: Double = 0.0
    var k: Int = (0).toInt()
    while (k < 10) {
        var denom: Double = ((2 * k) + 1).toDouble()
        sum = sum + (term / denom)
        term = term * y2
        k = k + 1
    }
    return 2.0 * sum
}

fun exp(x: Double): Double {
    var term: Double = 1.0
    var sum: Double = 1.0
    var n: Int = (1).toInt()
    while (n < 20) {
        term = (term * x) / (n.toDouble())
        sum = sum + term
        n = n + 1
    }
    return sum
}

fun softplus(vector: MutableList<Double>): MutableList<Double> {
    var result: MutableList<Double> = mutableListOf<Double>()
    var i: Int = (0).toInt()
    while (i < vector.size) {
        var x: Double = vector[i]!!
        var value: Double = (kotlin.math.ln(1.0 + kotlin.math.exp(x))).toDouble()
        result = run { val _tmp = result.toMutableList(); _tmp.add(value); _tmp }
        i = i + 1
    }
    return result
}

fun user_main(): Unit {
    var v1: MutableList<Double> = mutableListOf(2.3, 0.6, 0.0 - 2.0, 0.0 - 3.8)
    var v2: MutableList<Double> = mutableListOf(0.0 - 9.2, 0.0 - 0.3, 0.45, 0.0 - 4.56)
    var r1: MutableList<Double> = softplus(v1)
    var r2: MutableList<Double> = softplus(v2)
    println(r1)
    println(r2)
}

fun main() {
    user_main()
}
