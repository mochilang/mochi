fun main() {
    val customers = mutableListOf(mutableMapOf("id" to 1, "name" to "Alice"), mutableMapOf("id" to 2, "name" to "Bob"))
    val orders = mutableListOf(mutableMapOf("id" to 100, "customerId" to 1), mutableMapOf("id" to 101, "customerId" to 2))
    val items = mutableListOf(mutableMapOf("orderId" to 100, "sku" to "a"), mutableMapOf("orderId" to 101, "sku" to "b"))
    val result = run {
    val _res = mutableListOf<MutableMap<String, Any>>()
    for (o in orders) {
        for (c in customers) {
            for (i in items) {
                if ((o["customerId"] == c["id"]) && (o["id"] == i["orderId"])) {
                    _res.add(mutableMapOf("name" to c["name"], "sku" to i["sku"]))
                }
            }
        }
    }
    _res
}
    println("--- Multi Join ---")
    for (r in result) {
        println(listOf(r["name"], "bought item", r["sku"]).joinToString(" "))
    }
}
