data class Result(var orderId: Any?, var orderCustomerId: Any?, var pairedCustomerName: Any?, var orderTotal: Any?)

data class Customer(var id: Int, var name: String)

data class Order(var id: Int, var customerId: Int, var total: Int)

val customers = mutableListOf(Customer(id = 1, name = "Alice"), Customer(id = 2, name = "Bob"), Customer(id = 3, name = "Charlie"))

val orders = mutableListOf(Order(id = 100, customerId = 1, total = 250), Order(id = 101, customerId = 2, total = 125), Order(id = 102, customerId = 1, total = 300))

val result = run {
    val __res = mutableListOf<Result>()
    for (o in orders) {
        for (c in customers) {
            __res.add(Result(orderId = o.id, orderCustomerId = o.customerId, pairedCustomerName = c.name, orderTotal = o.total))
        }
    }
    __res
}

fun main() {
    println("--- Cross Join: All order-customer pairs ---")
    for (entry in result) {
        println(listOf("Order", entry.orderId, "(customerId:", entry.orderCustomerId, ", total: $", entry.orderTotal, ") paired with", entry.pairedCustomerName).joinToString(" "))
    }
}
