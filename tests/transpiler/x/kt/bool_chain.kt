// Mochi 0.10.31 - generated 2025-07-19 13:22:10 UTC
fun boom(): Boolean {
    println("boom")
    return true
}

fun main() {
    println((((1 < 2) && (2 < 3)) && (3 < 4)))
    println((((1 < 2) && (2 > 3)) && boom()))
    println(((((1 < 2) && (2 < 3)) && (3 > 4)) && boom()))
}
