// Code generated from tests/vm/valid/tail_recursion.mochi

/**
 * Auto-generated from Mochi
 * @param n Int
 * @param acc Int
 * @return Int
 */
tailrec fun sum_rec(n: Int, acc: Int): Int {
    if (n == 0) {
        return acc
    }
    return sum_rec(n - 1, acc + n)
}

fun main() {
    println(sum_rec(10, 0))
}
