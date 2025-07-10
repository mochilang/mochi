data class Customer(val id: Int, val name: String)
data class Order(val id: Int, val customerId: Int)

data class Stat(val name: String, val count: Int)

fun main() {
    val customers = listOf(
        Customer(1, "Alice"),
        Customer(2, "Bob"),
        Customer(3, "Charlie")
    )
    val orders = listOf(
        Order(100, 1),
        Order(101, 1),
        Order(102, 2)
    )
    val joined = customers.flatMap { c ->
        val os = orders.filter { it.customerId == c.id }
        if (os.isEmpty()) listOf(c to null) else os.map { c to it }
    }
    val stats = joined.groupBy { it.first.name }
        .map { (name, pairs) ->
            val count = pairs.count { it.second != null }
            Stat(name, count)
        }
    println("--- Group Left Join ---")
    for (s in stats) {
        println("${s.name} orders: ${s.count}")
    }
}
