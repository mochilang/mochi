fun main() {
    var scores: MutableMap<String, Int> = mutableMapOf("alice" to 1)
    scores["bob"] = 2
    println(scores["bob"])
}
