data class Customer(var id: Int, var name: String)

data class Order(var id: Int, var customerId: Int, var total: Int)

data class Result(var orderId: Any?, var customer: Any?, var total: Any?)

val customers = mutableListOf(Customer(id = 1, name = "Alice"), Customer(id = 2, name = "Bob"))

val orders = mutableListOf(Order(id = 100, customerId = 1, total = 250), Order(id = 101, customerId = 3, total = 80))

val result = run {
    val __res = mutableListOf<Result>()
    for (o in orders) {
        for (c in customers) {
            if (o.customerId == c.id) {
                __res.add(Result(orderId = o.id, customer = c, total = o.total))
            }
        }
    }
    __res
}

fun main() {
    println("--- Left Join ---")
    for (entry in result) {
        println(listOf("Order", entry.orderId, "customer", entry.customer, "total", entry.total).joinToString(" "))
    }
}
