fun toBool(v: Any?): Boolean = when (v) {
    is Boolean -> v
    is Int -> v != 0
    is Double -> v != 0.0
    is String -> v.isNotEmpty()
    null -> false
    else -> true
}
data class Customer(var id: Int, var name: String)

data class Order(var id: Int, var customerId: Int, var total: Int)

val customers = mutableListOf(Customer(id = 1, name = "Alice"), Customer(id = 2, name = "Bob"), Customer(id = 3, name = "Charlie"))

val orders = mutableListOf(Order(id = 100, customerId = 1, total = 250), Order(id = 101, customerId = 2, total = 125), Order(id = 102, customerId = 1, total = 300), Order(id = 103, customerId = 4, total = 80))

val result = run {
    val __res = mutableListOf<MutableMap<Any?, Any?>>()
    for (o in orders) {
        for (c in customers) {
            if (toBool(o.customerId == c.id)) {
                __res.add((mutableMapOf("orderId" to o.id, "customerName" to c.name, "total" to o.total) as MutableMap<Any?, Any?>))
            }
        }
    }
    __res
}

fun main() {
    println("--- Orders with customer info ---")
    for (entry in result) {
        println(listOf("Order", (entry as MutableMap<*, *>)["orderId"], "by", (entry as MutableMap<*, *>)["customerName"], "- $", (entry as MutableMap<*, *>)["total"]).joinToString(" "))
    }
}
