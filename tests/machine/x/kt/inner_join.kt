data class Customer(val id: Int, val name: String)
data class Order(val id: Int, val customerId: Int, val total: Int)

data class Result(val orderId: Int, val customerName: String, val total: Int)

fun main() {
    val customers = listOf(
        Customer(1, "Alice"),
        Customer(2, "Bob"),
        Customer(3, "Charlie")
    )
    val orders = listOf(
        Order(100, 1, 250),
        Order(101, 2, 125),
        Order(102, 1, 300),
        Order(103, 4, 80)
    )
    val result = orders.mapNotNull { o ->
        val c = customers.find { it.id == o.customerId }
        c?.let { Result(o.id, it.name, o.total) }
    }
    println("--- Orders with customer info ---")
    for (entry in result) {
        println("Order ${entry.orderId} by ${entry.customerName} - $${entry.total}")
    }
}
