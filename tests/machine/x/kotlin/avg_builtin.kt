fun toDouble(v: Any?): Double = when (v) {
    is Double -> v
    is Int -> v.toDouble()
    is String -> v.toDouble()
    else -> 0.0
}
fun main() {
    println(mutableListOf(1, 2, 3).map{ toDouble(it) }.average())
}
