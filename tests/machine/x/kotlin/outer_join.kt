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

data class Result(var order: Any?, var customer: Any?)

val customers = mutableListOf(Customer(id = 1, name = "Alice"), Customer(id = 2, name = "Bob"), Customer(id = 3, name = "Charlie"), Customer(id = 4, name = "Diana"))

val orders = mutableListOf(Order(id = 100, customerId = 1, total = 250), Order(id = 101, customerId = 2, total = 125), Order(id = 102, customerId = 1, total = 300), Order(id = 103, customerId = 5, total = 80))

val result = run {
    val __res = mutableListOf<Result>()
    val __matched = mutableSetOf<Any?>()
    for (o in orders) {
        val __tmp = mutableListOf<Any?>()
        for (c in customers) {
            if (o.customerId == c.id) {
                __tmp.add(c)
                __matched.add(c)
            }
        }
        if (__tmp.isEmpty()) __tmp.add(null)
        for (c in __tmp) {
            __res.add(Result(order = o, customer = c))
        }
    }
    for (c in customers) {
        if (!__matched.contains(c)) {
            val o = null
            __res.add(Result(order = o, customer = c))
        }
    }
    __res
}

fun main() {
    println("--- Outer Join using syntax ---")
    for (row in result) {
        if (toBool(row.order)) {
            if (toBool(row.customer)) {
                println(listOf("Order", row.order.id, "by", row.customer.name, "- $", row.order.total).joinToString(" "))
            }
            else {
                println(listOf("Order", row.order.id, "by", "Unknown", "- $", row.order.total).joinToString(" "))
            }
        }
        else {
            println(listOf("Customer", row.customer.name, "has no orders").joinToString(" "))
        }
    }
}
