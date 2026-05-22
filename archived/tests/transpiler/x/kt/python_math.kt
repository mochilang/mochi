fun main() {
    val r: Double = 3.0
    val area: Double = (kotlin.math.PI as Number).toDouble() * (Math.pow(r, 2.0) as Number).toDouble()
    val root: Double = kotlin.math.sqrt(49.0)
    val sin45: Double = kotlin.math.sin((kotlin.math.PI as Number).toDouble() / 4.0)
    val log_e: Double = kotlin.math.ln(kotlin.math.E)
    println(listOf("Circle area with r =", r, "=>", area).joinToString(" "))
    println(listOf("Square root of 49:", root).joinToString(" "))
    println(listOf("sin(π/4):", sin45).joinToString(" "))
    println(listOf("log(e):", log_e).joinToString(" "))
}
