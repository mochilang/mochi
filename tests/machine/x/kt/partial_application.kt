fun add(a: Int, b: Int): Int = a + b

fun main() {
    val add5: (Int) -> Int = { b -> add(5, b) }
    println(add5(3))
}
