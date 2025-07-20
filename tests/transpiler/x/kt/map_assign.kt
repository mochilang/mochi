var scores: MutableMap<String, Int> = mutableMapOf("alice" to 1)
fun main() {
    scores["bob"] = 2
    println(scores["bob"])
}
