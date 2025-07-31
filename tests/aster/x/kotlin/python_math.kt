fun main() {
    val r: Double = float_literal
    val area: Double = (as_expression).toDouble() * (as_expression).toDouble()
    val root: Double = kotlin.math.sqrt(float_literal)
    val sin45: Double = kotlin.math.sin((as_expression).toDouble() / float_literal)
    val log_e: Double = kotlin.math.ln(kotlin.math.E)
    println(listOf("Circle area with r =", r, "=>", area).joinToString(" "))
    println(listOf("Square root of 49:", root).joinToString(" "))
    println(listOf("sin(π/4):", sin45).joinToString(" "))
    println(listOf("log(e):", log_e).joinToString(" "))
}
