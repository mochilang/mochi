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

fun main() {
    println(label)
}
