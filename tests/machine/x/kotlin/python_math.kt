object math {
    const val pi: Double = kotlin.math.PI
    const val e: Double = kotlin.math.E
    fun sqrt(x: Double): Double = kotlin.math.sqrt(x)
    fun pow(x: Double, y: Double): Double = kotlin.math.pow(x, y)
    fun sin(x: Double): Double = kotlin.math.sin(x)
    fun log(x: Double): Double = kotlin.math.ln(x)
}

val r = 3

val area = math.pi * math.pow(r, 2)

val root = math.sqrt(49)

val sin45 = math.sin(math.pi / 4)

val log_e = math.log(math.e)

fun main() {
    println(listOf("Circle area with r =", r, "=>", area).joinToString(" "))
    println(listOf("Square root of 49:", root).joinToString(" "))
    println(listOf("sin(π/4):", sin45).joinToString(" "))
    println(listOf("log(e):", log_e).joinToString(" "))
}
