// Code generated from tests/vm/valid/inner_join.mochi

data class Customer(var id: Int, var name: String)

data class Order(var id: Int, var customerId: Int, var total: Int)

data class Result(var orderId: Any?, var customerName: Any?, var total: Any?)

val customers = mutableListOf(Customer(id = 1, name = "Alice"), Customer(id = 2, name = "Bob"), Customer(id = 3, name = "Charlie"))

val orders = mutableListOf(Order(id = 100, customerId = 1, total = 250), Order(id = 101, customerId = 2, total = 125), Order(id = 102, customerId = 1, total = 300), Order(id = 103, customerId = 4, total = 80))

val result = run {
    val __res = mutableListOf<Result>()
    for (o in orders) {
        for (c in customers) {
            if (o.customerId == c.id) {
                __res.add(Result(orderId = o.id, customerName = c.name, total = o.total))
            }
        }
    }
    __res
}

fun main() {
    println("--- Orders with customer info ---")
    for (entry in result) {
        println(listOf("Order", entry.orderId, "by", entry.customerName, "- $", entry.total).joinToString(" "))
    }
}
