object testpkg {
    fun Add(a: Int, b: Int): Int = a + b
    const val Pi: Double = 3.14
    var Answer: Int = 42
}

fun main() {
    println(testpkg.Add(2, 3))
    println(testpkg.Pi)
    println(testpkg.Answer)
}
