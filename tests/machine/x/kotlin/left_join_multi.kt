data class Customer(var id: Int, var name: String)

data class Order(var id: Int, var customerId: Int)

data class Item(var orderId: Int, var sku: String)

data class Result(var orderId: Any?, var name: Any?, var item: Any?)

val customers = mutableListOf(Customer(id = 1, name = "Alice"), Customer(id = 2, name = "Bob"))

val orders = mutableListOf(Order(id = 100, customerId = 1), Order(id = 101, customerId = 2))

val items = mutableListOf(Item(orderId = 100, sku = "a"))

val result = run {
    val __res = mutableListOf<Result>()
    for (o in orders) {
        for (c in customers) {
            if (o.customerId == c.id) {
                for (i in items) {
                    if (o.id == i.orderId) {
                        __res.add(Result(orderId = o.id, name = c.name, item = i))
                    }
                }
            }
        }
    }
    __res
}

fun main() {
    println("--- Left Join Multi ---")
    for (r in result) {
        println(listOf(r.orderId, r.name, r.item).joinToString(" "))
    }
}
