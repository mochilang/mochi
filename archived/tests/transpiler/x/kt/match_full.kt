fun classify(n: Int): String {
    return when (n) {
    0 -> "zero"
    1 -> "one"
    else -> "many"
}
}

fun main() {
    val x: Int = 2
    val label: String = when (x) {
    1 -> "one"
    2 -> "two"
    3 -> "three"
    else -> "unknown"
}
    println(label)
    val day: String = "sun"
    val mood: String = when (day) {
    "mon" -> "tired"
    "fri" -> "excited"
    "sun" -> "relaxed"
    else -> "normal"
}
    println(mood)
    val ok: Boolean = true
    val status: String = when (ok) {
    true -> "confirmed"
    false -> "denied"
}
    println(status)
    println(classify(0))
    println(classify(5))
}
