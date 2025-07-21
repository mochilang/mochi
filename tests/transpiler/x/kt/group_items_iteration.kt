data class GGroup(val key: Any, val items: MutableList<MutableMap<String, MutableMap<String, Any>>>)
fun main() {
    val data: MutableList<MutableMap<String, Any>> = mutableListOf(mutableMapOf<String, Any>("tag" to "a", "val" to 1), mutableMapOf<String, Any>("tag" to "a", "val" to 2), mutableMapOf<String, Any>("tag" to "b", "val" to 3))
    val groups: MutableList<Any> = run {
    val _groups = mutableMapOf<Any, MutableList<MutableMap<String, MutableMap<String, Any>>>>()
    for (d in data) {
        val _list = _groups.getOrPut((d["tag"]!!)) { mutableListOf<MutableMap<String, MutableMap<String, Any>>>() }
        _list.add(mutableMapOf<String, MutableMap<String, Any>>("d" to d))
    }
    val _res = mutableListOf<Any>()
    for ((key, items) in _groups) {
        val g = GGroup(key, items)
        _res.add(g)
    }
    _res
}
    var tmp: MutableList<Any> = mutableListOf()
    for (g in groups) {
        var total = 0
        for (x in g.items) {
            total = (total as Number).toDouble() + ((x["val"]!!) as Number).toDouble()
        }
        tmp = tmp + mutableMapOf<String, Any>("tag" to g.key, "total" to total)
    }
    val result: MutableList<Any> = run {
    val _tmp = mutableListOf<Pair<Any, Any>>()
    for (r in tmp) {
        _tmp.add(Pair(r.tag, r))
    }
    val _res = _tmp.sortedBy { it.first }.map { it.second }.toMutableList()
    _res
}
    println(result)
}
