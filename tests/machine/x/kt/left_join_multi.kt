data class Customer(val id: Int, val name: String)
data class Order(val id: Int, val customerId: Int)
data class Item(val orderId: Int, val sku: String)

data class Joined(val orderId: Int, val name: String, val item: Item?)

fun main() {
    val customers = listOf(
        Customer(1, "Alice"),
        Customer(2, "Bob")
    )
    val orders = listOf(
        Order(100, 1),
        Order(101, 2)
    )
    val items = listOf(
        Item(100, "a")
    )

    val result = mutableListOf<Joined>()
    for (o in orders) {
        val c = customers.find { it.id == o.customerId } ?: continue
        val isForOrder = items.filter { it.orderId == o.id }
        if (isForOrder.isEmpty()) {
            result.add(Joined(o.id, c.name, null))
        } else {
            for (i in isForOrder) {
                result.add(Joined(o.id, c.name, i))
            }
        }
    }
    println("--- Left Join Multi ---")
    for (r in result) {
        println("${r.orderId} ${r.name} ${r.item}")
    }
}
