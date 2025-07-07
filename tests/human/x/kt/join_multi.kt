data class Customer(val id: Int, val name: String)
data class Order(val id: Int, val customerId: Int)
data class Item(val orderId: Int, val sku: String)

data class Res(val name: String, val sku: String)

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
        Item(100, "a"),
        Item(101, "b")
    )

    val result = mutableListOf<Res>()
    for (o in orders) {
        val c = customers.find { it.id == o.customerId } ?: continue
        val its = items.filter { it.orderId == o.id }
        for (i in its) {
            result.add(Res(c.name, i.sku))
        }
    }
    println("--- Multi Join ---")
    for (r in result) {
        println("${r.name} bought item ${r.sku}")
    }
}
