fun toBool(v: Any?): Boolean = when (v) {
    is Boolean -> v
    is Int -> v != 0
    is Double -> v != 0.0
    is String -> v.isNotEmpty()
    null -> false
    else -> true
}
val customers = mutableListOf(mutableMapOf("id" to 1, "name" to "Alice"), mutableMapOf("id" to 2, "name" to "Bob"), mutableMapOf("id" to 3, "name" to "Charlie"), mutableMapOf("id" to 4, "name" to "Diana"))

val orders = mutableListOf(mutableMapOf("id" to 100, "customerId" to 1, "total" to 250), mutableMapOf("id" to 101, "customerId" to 2, "total" to 125), mutableMapOf("id" to 102, "customerId" to 1, "total" to 300))

val result = run {
    val __res = mutableListOf<MutableMap<Any?, Any?>>()
    for (c in customers) {
        for (o in orders) {
            if (toBool((o as MutableMap<*, *>)["customerId"] == (c as MutableMap<*, *>)["id"])) {
                __res.add((mutableMapOf("customerName" to (c as MutableMap<*, *>)["name"], "order" to o) as MutableMap<Any?, Any?>))
            }
        }
    }
    __res
}

fun main() {
    println("--- Right Join using syntax ---")
    for (entry in result) {
        if (toBool((entry as MutableMap<*, *>)["order"])) {
            println(listOf("Customer", (entry as MutableMap<*, *>)["customerName"], "has order", (entry as MutableMap<*, *>)["order"]["id"], "- $", (entry as MutableMap<*, *>)["order"]["total"]).joinToString(" "))
        }
        else {
            println(listOf("Customer", (entry as MutableMap<*, *>)["customerName"], "has no orders").joinToString(" "))
        }
    }
}
