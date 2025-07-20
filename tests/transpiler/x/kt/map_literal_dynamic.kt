fun main() {
    var x: Int = 3
    var y: Int = 4
    var m: MutableMap<String, Int> = mutableMapOf("a" to x, "b" to y)
    println((m["a"] + " ") + m["b"])
}
