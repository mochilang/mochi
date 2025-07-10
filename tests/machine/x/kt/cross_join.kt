data class Customer(val id: Int, val name: String)
data class Order(val id: Int, val customerId: Int, val total: Int)

fun main() {
    val customers = listOf(
        Customer(1, "Alice"),
        Customer(2, "Bob"),
        Customer(3, "Charlie")
    )
    val orders = listOf(
        Order(100, 1, 250),
        Order(101, 2, 125),
        Order(102, 1, 300)
    )
    println("--- Cross Join: All order-customer pairs ---")
    for (o in orders) {
        for (c in customers) {
            println("Order ${o.id} (customerId: ${o.customerId}, total: $${o.total}) paired with ${c.name}")
        }
    }
}
