fun toBool(v: Any?): Boolean = when (v) {
    is Boolean -> v
    is Int -> v != 0
    is Double -> v != 0.0
    is String -> v.isNotEmpty()
    null -> false
    else -> true
}
val customers = mutableListOf(mutableMapOf("id" to 1, "name" to "Alice"), mutableMapOf("id" to 2, "name" to "Bob"))

val orders = mutableListOf(mutableMapOf("id" to 100, "customerId" to 1, "total" to 250), mutableMapOf("id" to 101, "customerId" to 3, "total" to 80))

val result = run {
    val __res = mutableListOf<MutableMap<Any?, Any?>>()
    for (o in orders) {
        for (c in customers) {
            if (toBool((o as MutableMap<*, *>)["customerId"] == (c as MutableMap<*, *>)["id"])) {
                __res.add((mutableMapOf("orderId" to (o as MutableMap<*, *>)["id"], "customer" to c, "total" to (o as MutableMap<*, *>)["total"]) as MutableMap<Any?, Any?>))
            }
        }
    }
    __res
}

fun main() {
    println("--- Left Join ---")
    for (entry in result) {
        println(listOf("Order", (entry as MutableMap<*, *>)["orderId"], "customer", (entry as MutableMap<*, *>)["customer"], "total", (entry as MutableMap<*, *>)["total"]).joinToString(" "))
    }
}
