fun main() {
    val xs = listOf(1, 2, 3)
    val ys = xs.filter { it % 2 == 1 }
    println(1 in ys)
    println(2 in ys)

    val m = mapOf("a" to 1)
    println("a" in m)
    println("b" in m)

    val s = "hello"
    println("ell" in s)
    println("foo" in s)
}
