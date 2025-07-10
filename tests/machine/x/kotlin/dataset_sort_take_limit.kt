data class Product(var name: String, var price: Int)

val products = mutableListOf(Product(name = "Laptop", price = 1500), Product(name = "Smartphone", price = 900), Product(name = "Tablet", price = 600), Product(name = "Monitor", price = 300), Product(name = "Keyboard", price = 100), Product(name = "Mouse", price = 50), Product(name = "Headphones", price = 200))

val expensive = run {
    val __res = mutableListOf<Product>()
    for (p in products) {
        __res.add(p)
    }
    __res
}.sortedByDescending { it.price as Comparable<Any> }.drop(1).take(3)

fun main() {
    println("--- Top products (excluding most expensive) ---")
    for (item in expensive) {
        println(listOf((item as MutableMap<*, *>)["name"], "costs $", (item as MutableMap<*, *>)["price"]).joinToString(" "))
    }
}
