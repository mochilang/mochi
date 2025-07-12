// Code generated from tests/vm/valid/python_auto.mochi

object math {
    const val pi: Double = kotlin.math.PI
    const val e: Double = kotlin.math.E
    fun sqrt(x: Double): Double = kotlin.math.sqrt(x)
    fun pow(x: Double, y: Double): Double = Math.pow(x, y)
    fun sin(x: Double): Double = kotlin.math.sin(x)
    fun log(x: Double): Double = kotlin.math.ln(x)
}

fun main() {
    println(math.sqrt(16.0))
    println(math.pi)
}
