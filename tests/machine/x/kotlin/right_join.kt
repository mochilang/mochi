fun toBool(v: Any?): Boolean = when (v) {
    is Boolean -> v
    is Int -> v != 0
    is Double -> v != 0.0
    is String -> v.isNotEmpty()
    null -> false
    else -> true
}
data class Result(var customerName: Any?, var order: Any?)

data class Customer(var id: Int, var name: String)

data class Order(var id: Int, var customerId: Int, var total: Int)

val customers = mutableListOf(Customer(id = 1, name = "Alice"), Customer(id = 2, name = "Bob"), Customer(id = 3, name = "Charlie"), Customer(id = 4, name = "Diana"))

val orders = mutableListOf(Order(id = 100, customerId = 1, total = 250), Order(id = 101, customerId = 2, total = 125), Order(id = 102, customerId = 1, total = 300))

val result = run {
    val __res = mutableListOf<Result>()
    for (c in customers) {
        val __tmp = mutableListOf<Any?>()
        for (o in orders) {
            if (o.customerId == c.id) {
                __tmp.add(o)
            }
        }
        if (__tmp.isEmpty()) __tmp.add(null)
        for (o in __tmp) {
            __res.add(Result(customerName = c.name, order = o))
        }
    }
    __res
}

fun main() {
    println("--- Right Join using syntax ---")
    for (entry in result) {
        if (toBool(entry.order)) {
            println(listOf("Customer", entry.customerName, "has order", entry.order.id, "- $", entry.order.total).joinToString(" "))
        }
        else {
            println(listOf("Customer", entry.customerName, "has no orders").joinToString(" "))
        }
    }
}
