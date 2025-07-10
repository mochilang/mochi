val products = mutableListOf(mutableMapOf("name" to "Laptop", "price" to 1500), mutableMapOf("name" to "Smartphone", "price" to 900), mutableMapOf("name" to "Tablet", "price" to 600), mutableMapOf("name" to "Monitor", "price" to 300), mutableMapOf("name" to "Keyboard", "price" to 100), mutableMapOf("name" to "Mouse", "price" to 50), mutableMapOf("name" to "Headphones", "price" to 200))

val expensive = run {
    val __res = mutableListOf<MutableMap<String, Any?>>()
    for (p in products) {
        __res.add((p as MutableMap<String, Any?>))
    }
    __res
}.sortedByDescending { (it as MutableMap<*, *>)["price"] as Comparable<Any> }.drop(1).take(3)

fun main() {
    println("--- Top products (excluding most expensive) ---")
    for (item in expensive) {
        println(listOf((item as MutableMap<*, *>)["name"], "costs $", (item as MutableMap<*, *>)["price"]).joinToString(" "))
    }
}
