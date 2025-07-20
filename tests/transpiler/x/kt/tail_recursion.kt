fun sum_rec(n: Int, acc: Int): Int {
    if (n == 0) {
        return acc
    }
    return sum_rec(n - 1, acc + n)
}

fun main() {
    println(sum_rec(10, 0))
}
