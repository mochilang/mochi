fun main() {
    var x = 3
    var y = 4
    var m = mutableMapOf("a" to x, "b" to y)
    println(listOf(m["a"], m["b"]).joinToString(" "))
}
