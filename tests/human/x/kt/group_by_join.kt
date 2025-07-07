data class Customer(val id: Int, val name: String)
data class Order(val id: Int, val customerId: Int)

data class Stat(val name: String, val count: Int)

fun main() {
    val customers = listOf(
        Customer(1, "Alice"),
        Customer(2, "Bob")
    )
    val orders = listOf(
        Order(100, 1),
        Order(101, 1),
        Order(102, 2)
    )
    val stats = orders.mapNotNull { o ->
        val c = customers.find { it.id == o.customerId }
        c?.name
    }.groupingBy { it }.eachCount().map { Stat(it.key, it.value) }
    println("--- Orders per customer ---")
    for (s in stats) {
        println("${s.name} orders: ${s.count}")
    }
}
