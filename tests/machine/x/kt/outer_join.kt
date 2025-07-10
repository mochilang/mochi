data class Customer(val id: Int, val name: String)
data class Order(val id: Int, val customerId: Int, val total: Int)
data class Row(val order: Order?, val customer: Customer?)

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
        Order(102, 1, 300),
        Order(103, 5, 80)
    )
    val left = orders.map { o ->
        val c = customers.find { it.id == o.customerId }
        Row(o, c)
    }
    val rightOnly = customers.filter { c -> orders.none { it.customerId == c.id } }
        .map { c -> Row(null, c) }
    val result = left + rightOnly
    println("--- Outer Join using syntax ---")
    for (row in result) {
        if (row.order != null) {
            if (row.customer != null) {
                println("Order ${row.order.id} by ${row.customer.name} - $${row.order.total}")
            } else {
                println("Order ${row.order.id} by Unknown - $${row.order.total}")
            }
        } else {
            println("Customer ${row.customer!!.name} has no orders")
        }
    }
}
