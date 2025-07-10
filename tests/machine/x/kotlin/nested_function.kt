fun outer(x: Int): Int {
    fun inner(y: Int): Int {
        return x + y
    }
    return inner(5)
}

fun main() {
    println(outer(3))
}
