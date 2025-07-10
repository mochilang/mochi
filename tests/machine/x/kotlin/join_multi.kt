fun toBool(v: Any?): Boolean = when (v) {
    is Boolean -> v
    is Int -> v != 0
    is Double -> v != 0.0
    is String -> v.isNotEmpty()
    null -> false
    else -> true
}
val customers = mutableListOf(mutableMapOf("id" to 1, "name" to "Alice"), mutableMapOf("id" to 2, "name" to "Bob"))

val orders = mutableListOf(mutableMapOf("id" to 100, "customerId" to 1), mutableMapOf("id" to 101, "customerId" to 2))

val items = mutableListOf(mutableMapOf("orderId" to 100, "sku" to "a"), mutableMapOf("orderId" to 101, "sku" to "b"))

val result = run {
    val __res = mutableListOf<MutableMap<Any?, Any?>>()
    for (o in orders) {
        for (c in customers) {
            if (toBool((o as MutableMap<*, *>)["customerId"] == (c as MutableMap<*, *>)["id"])) {
                for (i in items) {
                    if (toBool((o as MutableMap<*, *>)["id"] == (i as MutableMap<*, *>)["orderId"])) {
                        __res.add((mutableMapOf("name" to (c as MutableMap<*, *>)["name"], "sku" to (i as MutableMap<*, *>)["sku"]) as MutableMap<Any?, Any?>))
                    }
                }
            }
        }
    }
    __res
}

fun main() {
    println("--- Multi Join ---")
    for (r in result) {
        println(listOf((r as MutableMap<*, *>)["name"], "bought item", (r as MutableMap<*, *>)["sku"]).joinToString(" "))
    }
}
