fun main() {
    val x = 2
    val label = when (x) {
        1 -> "one"
        2 -> "two"
        3 -> "three"
        else -> "unknown"
    }
    println(label)
}
