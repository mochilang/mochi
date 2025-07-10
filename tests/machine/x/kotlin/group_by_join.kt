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
data class Customer(var id: Int, var name: String)

data class Order(var id: Int, var customerId: Int)

val customers = mutableListOf(Customer(id = 1, name = "Alice"), Customer(id = 2, name = "Bob"))

val orders = mutableListOf(Order(id = 100, customerId = 1), Order(id = 101, customerId = 1), Order(id = 102, customerId = 2))

val stats = run {
    val __groups = mutableMapOf<Any?, Group>()
    val __order = mutableListOf<Any?>()
    for (o in orders) {
        for (c in customers) {
            if (toBool(o.customerId == c.id)) {
                val __k = c.name
                var __g = __groups[__k]
                if (__g == null) {
                    __g = Group(__k, mutableListOf())
                    __groups[__k] = __g
                    __order.add(__k)
                }
                __g.add(mutableMapOf("o" to o, "c" to c) as MutableMap<Any?, Any?>)
            }
        }
    }
    val __res = mutableListOf<MutableMap<Any?, Any?>>()
    for (k in __order) {
        val g = __groups[k]!!
        __res.add((mutableMapOf("name" to g.key, "count" to count(g)) as MutableMap<Any?, Any?>))
    }
    __res
}

fun main() {
    println("--- Orders per customer ---")
    for (s in stats) {
        println(listOf((s as MutableMap<*, *>)["name"], "orders:", (s as MutableMap<*, *>)["count"]).joinToString(" "))
    }
}
