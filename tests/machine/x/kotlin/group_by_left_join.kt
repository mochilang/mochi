fun count(list: Collection<Any?>): Int = list.size

fun toBool(v: Any?): Boolean = when (v) {
    is Boolean -> v
    is Int -> v != 0
    is Double -> v != 0.0
    is String -> v.isNotEmpty()
    null -> false
    else -> true
}

class Group(val key: Any?, val items: MutableList<Any?>) : MutableList<Any?> by items
val customers = mutableListOf(mutableMapOf("id" to 1, "name" to "Alice"), mutableMapOf("id" to 2, "name" to "Bob"), mutableMapOf("id" to 3, "name" to "Charlie"))

val orders = mutableListOf(mutableMapOf("id" to 100, "customerId" to 1), mutableMapOf("id" to 101, "customerId" to 1), mutableMapOf("id" to 102, "customerId" to 2))

val stats = run {
    val __groups = mutableMapOf<Any?, Group>()
    val __order = mutableListOf<Any?>()
    for (c in customers) {
        for (o in orders) {
            if (toBool((o as MutableMap<*, *>)["customerId"] == (c as MutableMap<*, *>)["id"])) {
                val __k = (c as MutableMap<*, *>)["name"]
                var __g = __groups[__k]
                if (__g == null) {
                    __g = Group(__k, mutableListOf())
                    __groups[__k] = __g
                    __order.add(__k)
                }
                __g.add(mutableMapOf("c" to c, "o" to o) as MutableMap<Any?, Any?>)
            }
        }
    }
    val __res = mutableListOf<MutableMap<Any?, Any?>>()
    for (k in __order) {
        val g = __groups[k]!!
        __res.add((mutableMapOf("name" to g.key, "count" to count(run {
    val __res = mutableListOf<MutableMap<String, Any?>>()
    for (r in g) {
        if (toBool((r as MutableMap<*, *>)["o"])) {
            __res.add((r as MutableMap<String, Any?>))
        }
    }
    __res
})) as MutableMap<Any?, Any?>))
    }
    __res
}

fun main() {
    println("--- Group Left Join ---")
    for (s in stats) {
        println(listOf((s as MutableMap<*, *>)["name"], "orders:", (s as MutableMap<*, *>)["count"]).joinToString(" "))
    }
}
