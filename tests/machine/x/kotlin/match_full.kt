val x = 2

val label = run {
    val __t = x
    when (__t) {
        1 -> "one"
        2 -> "two"
        3 -> "three"
        else -> "unknown"
    }
}

val day = "sun"

val mood = run {
    val __t = day
    when (__t) {
        "mon" -> "tired"
        "fri" -> "excited"
        "sun" -> "relaxed"
        else -> "normal"
    }
}

val ok = true

val status = run {
    val __t = ok
    when (__t) {
        true -> "confirmed"
        false -> "denied"
    }
}

fun classify(n: Int): String {
    return run {
    val __t = n
    when (__t) {
        0 -> "zero"
        1 -> "one"
        else -> "many"
    }
}
}

fun main() {
    println(label)
    println(mood)
    println(status)
    println(classify(0))
    println(classify(5))
}
