fun main() {
    val x: Int = 3
    val y: Int = 4
    val m: MutableMap<String, Int> = mutableMapOf("a" to x, "b" to y)
    println(listOf(m["a"], m["b"]).joinToString(" "))
}
