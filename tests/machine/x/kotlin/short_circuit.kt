/**
 * Auto-generated from Mochi
 * @param a Int
 * @param b Int
 * @return Boolean
 */
fun boom(a: Int, b: Int): Boolean {
    println("boom")
    return true
}

fun main() {
    println(false && boom(1, 2))
    println(true || boom(1, 2))
}
