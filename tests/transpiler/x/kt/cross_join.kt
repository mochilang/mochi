fun main() {
    val customers = mutableListOf(mutableMapOf("id" to 1, "name" to "Alice") as MutableMap<String, Any>, mutableMapOf("id" to 2, "name" to "Bob") as MutableMap<String, Any>, mutableMapOf("id" to 3, "name" to "Charlie") as MutableMap<String, Any>)
    val orders = mutableListOf(mutableMapOf("id" to 100, "customerId" to 1, "total" to 250) as MutableMap<String, Any>, mutableMapOf("id" to 101, "customerId" to 2, "total" to 125) as MutableMap<String, Any>, mutableMapOf("id" to 102, "customerId" to 1, "total" to 300) as MutableMap<String, Any>)
    val result = run {
    val _res = mutableListOf<MutableMap<String, Any>>()
    for (o in orders) {
        for (c in customers) {
            _res.add(mutableMapOf("orderId" to o["id"], "orderCustomerId" to o["customerId"], "pairedCustomerName" to c["name"], "orderTotal" to o["total"]) as MutableMap<String, Any>)
        }
    }
    _res
}
    println("--- Cross Join: All order-customer pairs ---")
    for (entry in result) {
        println(((((((((((((("Order" + " ") + entry["orderId"]) + " ") + "(customerId:") + " ") + entry["orderCustomerId"]) + " ") + ", total: $") + " ") + entry["orderTotal"]) + " ") + ") paired with") + " ") + entry["pairedCustomerName"])
    }
}
