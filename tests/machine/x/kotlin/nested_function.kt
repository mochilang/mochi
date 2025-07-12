/**
 * Auto-generated from Mochi
 * @param x Int
 * @return Int
 */
fun outer(x: Int): Int {
    /**
     * Auto-generated from Mochi
     * @param y Int
     * @return Int
     */
    fun inner(y: Int): Int {
        return x + y
    }
    return inner(5)
}

fun main() {
    println(outer(3))
}
