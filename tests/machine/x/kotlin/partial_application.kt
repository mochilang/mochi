// Code generated from tests/vm/valid/partial_application.mochi

val add5 = { p0: Int -> add(5, p0) }

/**
 * Auto-generated from Mochi
 * @param a Int
 * @param b Int
 * @return Int
 */
fun add(a: Int, b: Int): Int {
    return a + b
}

fun main() {
    println(add5(3))
}
