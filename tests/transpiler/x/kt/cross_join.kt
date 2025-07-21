fun main() {
    val customers: MutableList<MutableMap<String, Any>> = mutableListOf(mutableMapOf("id" to 1, "name" to "Alice"), mutableMapOf("id" to 2, "name" to "Bob"), mutableMapOf("id" to 3, "name" to "Charlie"))
    val orders: MutableList<MutableMap<String, Int>> = mutableListOf(mutableMapOf("id" to 100, "customerId" to 1, "total" to 250), mutableMapOf("id" to 101, "customerId" to 2, "total" to 125), mutableMapOf("id" to 102, "customerId" to 1, "total" to 300))
    val result: MutableList<MutableMap<String, Int>> = run {
    val _res = mutableListOf<MutableMap<String, Any>>()
    for (o in orders) {
        for (c in customers) {
            _res.add(mutableMapOf("orderId" to o["id"], "orderCustomerId" to o["customerId"], "pairedCustomerName" to c["name"], "orderTotal" to o["total"]))
        }
    }
    _res
}
    println("--- Cross Join: All order-customer pairs ---")
    for (entry in result) {
        println(listOf("Order", entry["orderId"], "(customerId:", entry["orderCustomerId"], ", total: $", entry["orderTotal"], ") paired with", entry["pairedCustomerName"]).joinToString(" "))
    }
}
