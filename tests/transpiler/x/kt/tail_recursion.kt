// Mochi 0.10.31 - generated 2025-07-19 14:33:26 UTC
fun sum_rec(n: Int, acc: Int): Int {
    if ((n == 0)) {
    return acc
}
    return sum_rec((n - 1), (acc + n))
}

fun main() {
    println(sum_rec(10, 0))
}
