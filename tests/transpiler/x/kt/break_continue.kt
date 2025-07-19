// Mochi 0.10.31 - generated 2025-07-19 12:51:09 UTC
fun main() {
    val numbers = listOf(1, 2, 3, 4, 5, 6, 7, 8, 9)
    for (n in numbers) {
    if (((n % 2) == 0)) {
    continue
}
    if ((n > 7)) {
    break
}
    println("odd number:", n)
}
}
