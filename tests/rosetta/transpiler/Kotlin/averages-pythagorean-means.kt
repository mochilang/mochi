fun powf(base: Double, exp: Int): Double {
    var result: Double = 1.0
    var i: Int = 0
    while (i < exp) {
        result = result * base
        i = i + 1
    }
    return result
}

fun nthRoot(x: Double, n: Int): Double {
    var low: Double = 0.0
    var high: Double = x
    var i: Int = 0
    while (i < 60) {
        val mid: Double = (low + high) / 2.0
        if (powf(mid, n) > x) {
            high = mid
        } else {
            low = mid
        }
        i = i + 1
    }
    return low
}

fun user_main(): Unit {
    var sum: Double = 0.0
    var sumRecip: Double = 0.0
    var prod: Double = 1.0
    var n: Int = 1
    while (n <= 10) {
        val f: Double = n.toDouble()
        sum = sum + f
        sumRecip = sumRecip + (1.0 / f)
        prod = prod * f
        n = n + 1
    }
    val count: Double = 10.0
    val a: Double = sum / count
    val g: Double = nthRoot(prod, 10)
    val h: Double = count / sumRecip
    println((((("A: " + a.toString()) + " G: ") + g.toString()) + " H: ") + h.toString())
    println("A >= G >= H: " + ((a >= g) && (g >= h)).toString())
}

fun main() {
    user_main()
}
