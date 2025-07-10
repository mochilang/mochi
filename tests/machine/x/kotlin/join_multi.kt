fun toBool(v: Any?): Boolean = when (v) {
    is Boolean -> v
    is Int -> v != 0
    is Double -> v != 0.0
    is String -> v.isNotEmpty()
    null -> false
    else -> true
}
data class Customer(var id: Int, var name: String)

data class Order(var id: Int, var customerId: Int)

data class Item(var orderId: Int, var sku: String)

data class Result(var name: Any?, var sku: Any?)

val customers = mutableListOf(Customer(id = 1, name = "Alice"), Customer(id = 2, name = "Bob"))

val orders = mutableListOf(Order(id = 100, customerId = 1), Order(id = 101, customerId = 2))

val items = mutableListOf(Item(orderId = 100, sku = "a"), Item(orderId = 101, sku = "b"))

val result = run {
    val __res = mutableListOf<Result>()
    for (o in orders) {
        for (c in customers) {
            if (toBool(o.customerId == c.id)) {
                for (i in items) {
                    if (toBool(o.id == i.orderId)) {
                        __res.add(Result(name = c.name, sku = i.sku))
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
        println(listOf(r.name, "bought item", r.sku).joinToString(" "))
    }
}
