data class Customer(val id: Int, val name: String)
data class Order(val id: Int, val customerId: Int, val total: Int)

data class Joined(val orderId: Int, val customer: Customer?, val total: Int)

fun main() {
    val customers = listOf(
        Customer(1, "Alice"),
        Customer(2, "Bob")
    )
    val orders = listOf(
        Order(100, 1, 250),
        Order(101, 3, 80)
    )
    val result = orders.map { o ->
        val c = customers.find { it.id == o.customerId }
        Joined(o.id, c, o.total)
    }
    println("--- Left Join ---")
    for (entry in result) {
        println("Order ${entry.orderId} customer ${entry.customer} total ${entry.total}")
    }
}
