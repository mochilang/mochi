// Code generated from tests/vm/valid/pure_global_fold.mochi

val k = 2

/**
 * Auto-generated from Mochi
 * @param x Int
 * @return Int
 */
fun inc(x: Int): Int {
    return x + k
}

fun main() {
    println(inc(3))
}
