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

data class Stat(var name: Any?, var count: Int)

val customers = mutableListOf(Customer(id = 1, name = "Alice"), Customer(id = 2, name = "Bob"), Customer(id = 3, name = "Charlie"))

val orders = mutableListOf(Order(id = 100, customerId = 1), Order(id = 101, customerId = 1), Order(id = 102, customerId = 2))

val stats = run {
    val __groups = mutableMapOf<Any?, Group>()
    val __order = mutableListOf<Any?>()
    for (c in customers) {
        for (o in orders) {
            if (o.customerId == c.id) {
                val __k = c.name
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
    val __res = mutableListOf<Stat>()
    for (k in __order) {
        val g = __groups[k]!!
        __res.add(Stat(name = g.key, count = run {
    val __res = mutableListOf<MutableMap<String, Any?>>()
    for (r in g) {
        if (toBool((r as MutableMap<String, Any?>)["o"])) {
            __res.add((r as MutableMap<String, Any?>))
        }
    }
    __res
}.size))
    }
    __res
}

fun main() {
    println("--- Group Left Join ---")
    for (s in stats) {
        println(listOf(s.name, "orders:", s.count).joinToString(" "))
    }
}
