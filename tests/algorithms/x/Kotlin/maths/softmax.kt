fun panic(msg: String): Nothing { throw RuntimeException(msg) }

fun exp_approx(x: Double): Double {
    var term: Double = 1.0
    var sum: Double = 1.0
    var i: Int = (1).toInt()
    while (i < 20) {
        term = (term * x) / (i.toDouble())
        sum = sum + term
        i = i + 1
    }
    return sum
}

fun softmax(vec: MutableList<Double>): MutableList<Double> {
    var exps: MutableList<Double> = mutableListOf<Double>()
    var i: Int = (0).toInt()
    while (i < vec.size) {
        exps = run { val _tmp = exps.toMutableList(); _tmp.add(exp_approx(vec[i]!!)); _tmp }
        i = i + 1
    }
    var total: Double = 0.0
    i = 0
    while (i < exps.size) {
        total = total + exps[i]!!
        i = i + 1
    }
    var result: MutableList<Double> = mutableListOf<Double>()
    i = 0
    while (i < exps.size) {
        result = run { val _tmp = result.toMutableList(); _tmp.add(exps[i]!! / total); _tmp }
        i = i + 1
    }
    return result
}

fun abs_val(x: Double): Double {
    if (x < 0.0) {
        return 0.0 - x
    }
    return x
}

fun approx_equal(a: Double, b: Double): Boolean {
    return abs_val(a - b) < 0.0001
}

fun test_softmax(): Unit {
    var s1: MutableList<Double> = softmax(mutableListOf(1.0, 2.0, 3.0, 4.0))
    var sum1: Double = 0.0
    var i: Int = (0).toInt()
    while (i < s1.size) {
        sum1 = sum1 + s1[i]!!
        i = i + 1
    }
    if (!approx_equal(sum1, 1.0)) {
        panic("sum test failed")
    }
    var s2: MutableList<Double> = softmax(mutableListOf(5.0, 5.0))
    if (!((approx_equal(s2[0]!!, 0.5) && approx_equal(s2[1]!!, 0.5)) as Boolean)) {
        panic("equal elements test failed")
    }
    var s3: MutableList<Double> = softmax(mutableListOf(0.0))
    if (!approx_equal(s3[0]!!, 1.0)) {
        panic("zero vector test failed")
    }
}

fun user_main(): Unit {
    test_softmax()
    println(softmax(mutableListOf(1.0, 2.0, 3.0, 4.0)).toString())
}

fun main() {
    user_main()
}
