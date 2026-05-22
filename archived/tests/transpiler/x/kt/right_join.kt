fun main() {
    val customers = mutableListOf(mutableMapOf("id" to 1, "name" to "Alice"), mutableMapOf("id" to 2, "name" to "Bob"), mutableMapOf("id" to 3, "name" to "Charlie"), mutableMapOf("id" to 4, "name" to "Diana"))
    val orders = mutableListOf(mutableMapOf("id" to 100, "customerId" to 1, "total" to 250), mutableMapOf("id" to 101, "customerId" to 2, "total" to 125), mutableMapOf("id" to 102, "customerId" to 1, "total" to 300))
    val result = run {
    val _res = mutableListOf<MutableMap<String, Any>>()
    for (o in orders) {
        var matched = false
        for (c in customers) {
            if (o.customerId == c["id"]) {
                matched = true
                _res.add(mutableMapOf("customerName" to c["name"], "order" to o))
            }
        }
        if (!matched) {
            val c: Any? = null
            _res.add(mutableMapOf("customerName" to c["name"], "order" to o))
        }
    }
    _res
}
    println("--- Right Join using syntax ---")
    for (entry in result) {
        if (entry["order"] != null) {
            println(listOf("Customer", entry["customerName"], "has order", entry["order"].id, "- $", entry["order"].total).joinToString(" "))
        } else {
            println(listOf("Customer", entry["customerName"], "has no orders").joinToString(" "))
        }
    }
}
