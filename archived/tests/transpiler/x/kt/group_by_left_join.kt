data class GGroup(val key: Any, val items: MutableList<MutableMap<String, Any>>)
fun main() {
    val customers = mutableListOf(mutableMapOf("id" to 1, "name" to "Alice"), mutableMapOf("id" to 2, "name" to "Bob"), mutableMapOf("id" to 3, "name" to "Charlie"))
    val orders = mutableListOf(mutableMapOf("id" to 100, "customerId" to 1), mutableMapOf("id" to 101, "customerId" to 1), mutableMapOf("id" to 102, "customerId" to 2))
    val stats = run {
    val _groups = mutableMapOf<Any, MutableList<MutableMap<String, Any>>>()
    for (c in customers) {
        for (o in orders) {
            if (o["customerId"] == c["id"]) {
            val _list = _groups.getOrPut(c["name"] as Any) { mutableListOf<MutableMap<String, Any>>() }
            _list.add(c)
            }
        }
    }
    val _res = mutableListOf<MutableMap<String, Any>>()
    for ((key, items) in _groups) {
        val g = GGroup(key, items)
        _res.add(mutableMapOf("name" to g.key, "count" to run {
    val _res = mutableListOf<MutableMap<String, Any>>()
    for (r in g.items) {
        if (r["o"]) {
            _res.add(r)
        }
    }
    _res
}.size))
    }
    _res
}
    println("--- Group Left Join ---")
    for (s in stats) {
        println(listOf(s["name"], "orders:", s["count"]).joinToString(" "))
    }
}
