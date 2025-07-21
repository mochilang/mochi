fun main() {
    val m = mutableMapOf("a" to 1, "b" to 2) as MutableMap<String, Any>
    json(m)
}
