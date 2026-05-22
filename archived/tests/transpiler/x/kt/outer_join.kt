fun main() {
    val customers = mutableListOf(mutableMapOf("id" to 1, "name" to "Alice"), mutableMapOf("id" to 2, "name" to "Bob"), mutableMapOf("id" to 3, "name" to "Charlie"), mutableMapOf("id" to 4, "name" to "Diana"))
    val orders = mutableListOf(mutableMapOf("id" to 100, "customerId" to 1, "total" to 250), mutableMapOf("id" to 101, "customerId" to 2, "total" to 125), mutableMapOf("id" to 102, "customerId" to 1, "total" to 300), mutableMapOf("id" to 103, "customerId" to 5, "total" to 80))
    val result = run {
    val _res = mutableListOf<MutableMap<String, Any>>()
    for (o in orders) {
        for (c in customers) {
            if (o["customerId"] == c["id"]) {
                _res.add(mutableMapOf("order" to o, "customer" to c))
            }
        }
    }
    _res
}
    println("--- Outer Join using syntax ---")
    for (row in result) {
        if (row["order"] != null) {
            if (row["customer"] != null) {
                println(listOf("Order", row["order"].id, "by", row["customer"].name, "- $", row["order"].total).joinToString(" "))
            } else {
                println(listOf("Order", row["order"].id, "by", "Unknown", "- $", row["order"].total).joinToString(" "))
            }
        } else {
            println(listOf("Customer", row["customer"].name, "has no orders").joinToString(" "))
        }
    }
}
