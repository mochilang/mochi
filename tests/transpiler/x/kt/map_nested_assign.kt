// Mochi 0.10.31 - generated 2025-07-19 14:33:24 UTC
fun main() {
    var data = mutableMapOf("outer" to mutableMapOf("inner" to 1))
    data["outer"]["inner"] = 2
    println(data["outer"]["inner"])
}
