val add5 = { p0: Int -> add(5, p0) }

fun add(a: Int, b: Int): Int {
    return a + b
}

fun main() {
    println(add5(3))
}
