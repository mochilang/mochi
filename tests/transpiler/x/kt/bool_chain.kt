// Mochi 0.10.31 - generated 2025-07-19 11:38:49 UTC
fun boom(): Any {
    println("boom")
    return true
}

fun main() {
    println((((1 < 2) && (2 < 3)) && (3 < 4)))
    println((((1 < 2) && (2 > 3)) && boom()))
    println(((((1 < 2) && (2 < 3)) && (3 > 4)) && boom()))
}
