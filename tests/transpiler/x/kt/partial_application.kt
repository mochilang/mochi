fun add(a: Int, b: Int): Int {
    return a + b
}

fun main() {
    val add5 = { p1: Int -> add(5, p1) }
    println(add5(3))
}
