fun main() {
    val customers = mutableListOf(mutableMapOf("id" to 1, "name" to "Alice"), mutableMapOf("id" to 2, "name" to "Bob"), mutableMapOf("id" to 3, "name" to "Charlie"))
    val orders = mutableListOf(mutableMapOf("id" to 100, "customerId" to 1, "total" to 250), mutableMapOf("id" to 101, "customerId" to 2, "total" to 125), mutableMapOf("id" to 102, "customerId" to 1, "total" to 300), mutableMapOf("id" to 103, "customerId" to 4, "total" to 80))
    val result = run {
    val _res = mutableListOf<MutableMap<String, Any>>()
    for (o in orders) {
        for (c in customers) {
            if (o["customerId"] == c["id"]) {
                _res.add(mutableMapOf("orderId" to o["id"], "customerName" to c["name"], "total" to o["total"]))
            }
        }
    }
    _res
}
    println("--- Orders with customer info ---")
    for (entry in result) {
        println(listOf("Order", entry["orderId"], "by", entry["customerName"], "- $", entry["total"]).joinToString(" "))
    }
}
