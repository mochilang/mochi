data class GGroup(val key: Any, val items: MutableList<MutableMap<String, Any>>)

fun main() {
    val people = mutableListOf(mutableMapOf("name" to "Alice", "age" to 30, "city" to "Paris"), mutableMapOf("name" to "Bob", "age" to 15, "city" to "Hanoi"), mutableMapOf("name" to "Charlie", "age" to 65, "city" to "Paris"), mutableMapOf("name" to "Diana", "age" to 45, "city" to "Hanoi"), mutableMapOf("name" to "Eve", "age" to 70, "city" to "Paris"), mutableMapOf("name" to "Frank", "age" to 22, "city" to "Hanoi"))
    val stats = run()
    println("--- People grouped by city ---")
    for (s in stats) {
        println(listOf(s["city"], ": count =", s["count"], ", avg_age =", s["avg_age"]).joinToString(" "))
    }
}
