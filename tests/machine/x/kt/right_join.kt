data class Customer(val id: Int, val name: String)
data class Order(val id: Int, val customerId: Int, val total: Int)

data class Entry(val customerName: String, val order: Order?)

fun main() {
    val customers = listOf(
        Customer(1, "Alice"),
        Customer(2, "Bob"),
        Customer(3, "Charlie"),
        Customer(4, "Diana")
    )
    val orders = listOf(
        Order(100, 1, 250),
        Order(101, 2, 125),
        Order(102, 1, 300)
    )
    val result = customers.map { c ->
        val o = orders.find { it.customerId == c.id }
        Entry(c.name, o)
    }
    println("--- Right Join using syntax ---")
    for (entry in result) {
        if (entry.order != null) {
            println("Customer ${entry.customerName} has order ${entry.order.id} - $${entry.order.total}")
        } else {
            println("Customer ${entry.customerName} has no orders")
        }
    }
}
