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

val customers = mutableListOf(Customer(id = 1, name = "Alice"), Customer(id = 2, name = "Bob"), Customer(id = 3, name = "Charlie"), Customer(id = 4, name = "Diana"))

val orders = mutableListOf(Order(id = 100, customerId = 1, total = 250), Order(id = 101, customerId = 2, total = 125), Order(id = 102, customerId = 1, total = 300), Order(id = 103, customerId = 5, total = 80))

val result = run {
    val __res = mutableListOf<MutableMap<Any?, Any?>>()
    val __matched = mutableSetOf<Any?>()
    for (o in orders) {
        val __tmp = mutableListOf<Any?>()
        for (c in customers) {
            if (toBool(o.customerId == c.id)) {
                __tmp.add(c)
                __matched.add(c)
            }
        }
        if (__tmp.isEmpty()) __tmp.add(null)
        for (c in __tmp) {
            __res.add(mutableMapOf("order" to o, "customer" to c))
        }
    }
    for (c in customers) {
        if (!__matched.contains(c)) {
            val o = null
            __res.add(mutableMapOf("order" to o, "customer" to c))
        }
    }
    __res
}

fun main() {
    println("--- Outer Join using syntax ---")
    for (row in result) {
        if (toBool((row as MutableMap<*, *>)["order"])) {
            if (toBool((row as MutableMap<*, *>)["customer"])) {
                println(listOf("Order", (row as MutableMap<*, *>)["order"]["id"], "by", (row as MutableMap<*, *>)["customer"]["name"], "- $", (row as MutableMap<*, *>)["order"]["total"]).joinToString(" "))
            }
            else {
                println(listOf("Order", (row as MutableMap<*, *>)["order"]["id"], "by", "Unknown", "- $", (row as MutableMap<*, *>)["order"]["total"]).joinToString(" "))
            }
        }
        else {
            println(listOf("Customer", (row as MutableMap<*, *>)["customer"]["name"], "has no orders").joinToString(" "))
        }
    }
}
