// Code generated from tests/vm/valid/bool_chain.mochi

/**
 * Auto-generated from Mochi
 * @return Boolean
 */
fun boom(): Boolean {
    println("boom")
    return true
}

fun main() {
    println((1 < 2) && (2 < 3) && (3 < 4))
    println((1 < 2) && (2 > 3) && boom())
    println((1 < 2) && (2 < 3) && (3 > 4) && boom())
}
