val PI: Double = 3.141592653589793
val dt: Double = 0.01
var s: Double = 0.0
var t1: Double = 0.0
var k1: Double = sinApprox(0.0) as Double
var i: Int = 1
var i2: Int = 1
fun sinApprox(x: float): float {
    var term: Double = x
    var sum: Double = x
    var n: Int = 1
    while (n <= 12) {
        val denom: Double = (2 * n) * ((2 * n) + 1).toDouble()
        term = (((0.0 - term) * x) * x) / denom
        sum = sum + term
        n = n + 1
    }
    return sum.toDouble()
}

fun main() {
    while (i <= 200) {
        val t2: Double = i.toDouble() * dt
        val k2: Double = sinApprox(t2 * PI) as Double
        s = s + (((k1 + k2) * 0.5) * (t2 - t1))
        t1 = t2
        k1 = k2
        i = i + 1
    }
    while (i2 <= 50) {
        val t2: Double = 2.0 + (i2.toDouble() * dt)
        val k2: Double = 0.0
        s = s + (((k1 + k2) * 0.5) * (t2 - t1))
        t1 = t2
        k1 = k2
        i2 = i2 + 1
    }
    println(s)
}
