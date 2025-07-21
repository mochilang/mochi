data class GGroup(val key: Any, val items: MutableList<MutableMap<String, Any>>)
fun main() {
    val nations: MutableList<MutableMap<String, Any>> = mutableListOf(mutableMapOf<String, Any>("id" to 1, "name" to "A"), mutableMapOf<String, Any>("id" to 2, "name" to "B"))
    val suppliers: MutableList<MutableMap<String, Int>> = mutableListOf(mutableMapOf<String, Int>("id" to 1, "nation" to 1), mutableMapOf<String, Int>("id" to 2, "nation" to 2))
    val partsupp: MutableList<MutableMap<String, Any>> = mutableListOf(mutableMapOf<String, Any>("part" to 100, "supplier" to 1, "cost" to 10.0, "qty" to 2), mutableMapOf<String, Any>("part" to 100, "supplier" to 2, "cost" to 20.0, "qty" to 1), mutableMapOf<String, Any>("part" to 200, "supplier" to 1, "cost" to 5.0, "qty" to 3))
    val filtered: MutableList<MutableMap<String, Any>> = run {
    val _res = mutableListOf<MutableMap<String, Any>>()
    for (ps in partsupp) {
        for (s in suppliers) {
            for (n in nations) {
                if (((s["id"]!! == ps["supplier"]!!) && (n["id"]!! == s["nation"]!!)) && (n["name"]!! == "A")) {
                    _res.add(mutableMapOf<String, Any>("part" to ps["part"]!!, "value" to (ps["cost"]!! as Number).toDouble() * (ps["qty"]!! as Number).toDouble()))
                }
            }
        }
    }
    _res
}
    val grouped: MutableList<MutableMap<String, Any>> = run {
    val _groups = mutableMapOf<Any, MutableList<MutableMap<String, Any>>>()
    for (x in filtered) {
        val _list = _groups.getOrPut(x["part"]!! as Any) { mutableListOf<MutableMap<String, Any>>() }
        _list.add(x)
    }
    val _res = mutableListOf<MutableMap<String, Any>>()
    for ((key, items) in _groups) {
        val g = GGroup(key, items)
        _res.add(mutableMapOf<String, Any>("part" to g.key, "total" to (run {
    val _res = mutableListOf<Any>()
    for (r in g.items) {
        _res.add(r["value"]!!)
    }
    _res
}.map{(it as Number).toDouble()}).sum()))
    }
    _res
}
    println(grouped)
}
