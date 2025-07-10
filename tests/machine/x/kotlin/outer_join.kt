fun toBool(v: Any?): Boolean = when (v) {
    is Boolean -> v
    is Int -> v != 0
    is Double -> v != 0.0
    is String -> v.isNotEmpty()
    null -> false
    else -> true
}
val customers = mutableListOf(mutableMapOf("id" to 1, "name" to "Alice"), mutableMapOf("id" to 2, "name" to "Bob"), mutableMapOf("id" to 3, "name" to "Charlie"), mutableMapOf("id" to 4, "name" to "Diana"))

val orders = mutableListOf(mutableMapOf("id" to 100, "customerId" to 1, "total" to 250), mutableMapOf("id" to 101, "customerId" to 2, "total" to 125), mutableMapOf("id" to 102, "customerId" to 1, "total" to 300), mutableMapOf("id" to 103, "customerId" to 5, "total" to 80))

val result = run {
    val __res = mutableListOf<MutableMap<Any?, Any?>>()
    val __matched = mutableSetOf<Any?>()
    for (o in orders) {
        val __tmp = mutableListOf<Any?>()
        for (c in customers) {
            if (toBool((o as MutableMap<*, *>)["customerId"] == (c as MutableMap<*, *>)["id"])) {
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
