data class GGroup(val key: Any, val items: MutableList<MutableMap<String, Any>>)

fun main() {
    val items = mutableListOf(mutableMapOf("cat" to "a", "val" to 10, "flag" to true), mutableMapOf("cat" to "a", "val" to 5, "flag" to false), mutableMapOf("cat" to "b", "val" to 20, "flag" to true))
    val result = run()
    println(result)
}
