val customers = mutableListOf(mutableMapOf("id" to 1, "name" to "Alice"), mutableMapOf("id" to 2, "name" to "Bob"), mutableMapOf("id" to 3, "name" to "Charlie"))

val orders = mutableListOf(mutableMapOf("id" to 100, "customerId" to 1, "total" to 250), mutableMapOf("id" to 101, "customerId" to 2, "total" to 125), mutableMapOf("id" to 102, "customerId" to 1, "total" to 300))

val result = run {
    val __res = mutableListOf<MutableMap<Any?, Any?>>()
    for (o in orders) {
        for (c in customers) {
            __res.add((mutableMapOf("orderId" to (o as MutableMap<*, *>)["id"], "orderCustomerId" to (o as MutableMap<*, *>)["customerId"], "pairedCustomerName" to (c as MutableMap<*, *>)["name"], "orderTotal" to (o as MutableMap<*, *>)["total"]) as MutableMap<Any?, Any?>))
        }
    }
    __res
}

fun main() {
    println("--- Cross Join: All order-customer pairs ---")
    for (entry in result) {
        println(listOf("Order", (entry as MutableMap<*, *>)["orderId"], "(customerId:", (entry as MutableMap<*, *>)["orderCustomerId"], ", total: $", (entry as MutableMap<*, *>)["orderTotal"], ") paired with", (entry as MutableMap<*, *>)["pairedCustomerName"]).joinToString(" "))
    }
}
