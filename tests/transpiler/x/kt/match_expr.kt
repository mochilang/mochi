fun main() {
    val x: Int = 2
    val label: String = when (x) {
    1 -> "one"
    2 -> "two"
    3 -> "three"
    else -> "unknown"
}
    println(label)
}
