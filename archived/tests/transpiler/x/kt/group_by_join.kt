data class GGroup(val key: Any, val items: MutableList<MutableMap<String, Int>>)
fun main() {
    val customers = mutableListOf(mutableMapOf("id" to 1, "name" to "Alice"), mutableMapOf("id" to 2, "name" to "Bob"))
    val orders = mutableListOf(mutableMapOf("id" to 100, "customerId" to 1), mutableMapOf("id" to 101, "customerId" to 1), mutableMapOf("id" to 102, "customerId" to 2))
    val stats = run {
    val _groups = mutableMapOf<Any, MutableList<MutableMap<String, Int>>>()
    for (o in orders) {
        for (c in customers) {
            if (o["customerId"] == c["id"]) {
            val _list = _groups.getOrPut(c["name"] as Any) { mutableListOf<MutableMap<String, Int>>() }
            _list.add(o)
            }
        }
    }
    val _res = mutableListOf<MutableMap<String, Any>>()
    for ((key, items) in _groups) {
        val g = GGroup(key, items)
        _res.add(mutableMapOf("name" to g.key, "count" to g.items.size))
    }
    _res
}
    println("--- Orders per customer ---")
    for (s in stats) {
        println(listOf(s["name"], "orders:", s["count"]).joinToString(" "))
    }
}
