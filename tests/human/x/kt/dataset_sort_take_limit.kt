data class Product(val name: String, val price: Int)

fun main() {
    val products = listOf(
        Product("Laptop", 1500),
        Product("Smartphone", 900),
        Product("Tablet", 600),
        Product("Monitor", 300),
        Product("Keyboard", 100),
        Product("Mouse", 50),
        Product("Headphones", 200)
    )
    val expensive = products.sortedByDescending { it.price }
        .drop(1)
        .take(3)
    println("--- Top products (excluding most expensive) ---")
    for (p in expensive) {
        println("${p.name} costs $${p.price}")
    }
}
