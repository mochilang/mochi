fun classify(n: Int): String = when (n) {
    0 -> "zero"
    1 -> "one"
    else -> "many"
}

fun main() {
    val x = 2
    val label = when (x) {
        1 -> "one"
        2 -> "two"
        3 -> "three"
        else -> "unknown"
    }
    println(label)

    val day = "sun"
    val mood = when (day) {
        "mon" -> "tired"
        "fri" -> "excited"
        "sun" -> "relaxed"
        else -> "normal"
    }
    println(mood)

    val ok = true
    val status = if (ok) "confirmed" else "denied"
    println(status)

    println(classify(0))
    println(classify(5))
}
