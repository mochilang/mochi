fun main() {
    val customers = mutableListOf(mutableMapOf("id" to 1, "name" to "Alice"), mutableMapOf("id" to 2, "name" to "Bob"))
    val orders = mutableListOf(mutableMapOf("id" to 100, "customerId" to 1, "total" to 250), mutableMapOf("id" to 101, "customerId" to 3, "total" to 80))
    val result = run {
    val _res = mutableListOf<MutableMap<String, Any>>()
    for (o in orders) {
        for (c in customers) {
            if (o["customerId"] == c["id"]) {
                _res.add(mutableMapOf("orderId" to o["id"], "customer" to c, "total" to o["total"]))
            }
        }
    }
    _res
}
    println("--- Left Join ---")
    for (entry in result) {
        println(listOf("Order", entry["orderId"], "customer", entry["customer"], "total", entry["total"]).joinToString(" "))
    }
}
