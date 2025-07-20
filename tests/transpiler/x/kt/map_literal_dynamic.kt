var x: Int = 3
var y: Int = 4
var m: MutableMap<String, Int> = mutableMapOf("a" to x, "b" to y)
fun main() {
    println((m["a"].toString() + " ") + m["b"].toString())
}
