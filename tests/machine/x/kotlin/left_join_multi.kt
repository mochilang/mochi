fun toBool(v: Any?): Boolean = when (v) {
    is Boolean -> v
    is Int -> v != 0
    is Double -> v != 0.0
    is String -> v.isNotEmpty()
    null -> false
    else -> true
}
data class Customer(var id: Int, var name: String)

data class Order(var id: Int, var customerId: Int)

data class Item(var orderId: Int, var sku: String)

val customers = mutableListOf(Customer(id = 1, name = "Alice"), Customer(id = 2, name = "Bob"))

val orders = mutableListOf(Order(id = 100, customerId = 1), Order(id = 101, customerId = 2))

val items = mutableListOf(Item(orderId = 100, sku = "a"))

val result = run {
    val __res = mutableListOf<MutableMap<Any?, Any?>>()
    for (o in orders) {
        for (c in customers) {
            if (toBool(o.customerId == c.id)) {
                for (i in items) {
                    if (toBool(o.id == i.orderId)) {
                        __res.add((mutableMapOf("orderId" to o.id, "name" to c.name, "item" to i) as MutableMap<Any?, Any?>))
                    }
                }
            }
        }
    }
    __res
}

fun main() {
    println("--- Left Join Multi ---")
    for (r in result) {
        println(listOf((r as MutableMap<*, *>)["orderId"], (r as MutableMap<*, *>)["name"], (r as MutableMap<*, *>)["item"]).joinToString(" "))
    }
}
