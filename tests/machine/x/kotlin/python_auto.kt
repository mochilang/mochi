object math {
    const val pi: Double = kotlin.math.PI
    const val e: Double = kotlin.math.E
    fun sqrt(x: Double): Double = kotlin.math.sqrt(x)
    fun pow(x: Double, y: Double): Double = kotlin.math.pow(x, y)
    fun sin(x: Double): Double = kotlin.math.sin(x)
    fun log(x: Double): Double = kotlin.math.ln(x)
}

fun main() {
    println(math.sqrt(16))
    println(math.pi)
}
