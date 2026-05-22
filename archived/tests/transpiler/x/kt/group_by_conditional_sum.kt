data class GGroup(val key: Any, val items: MutableList<MutableMap<String, Any>>)
fun main() {
    val items = mutableListOf(mutableMapOf("cat" to "a", "val" to 10, "flag" to true), mutableMapOf("cat" to "a", "val" to 5, "flag" to false), mutableMapOf("cat" to "b", "val" to 20, "flag" to true))
    val result = run {
    val _groups = mutableMapOf<Any, MutableList<MutableMap<String, Any>>>()
    for (i in items) {
        val _list = _groups.getOrPut(i["cat"] as Any) { mutableListOf<MutableMap<String, Any>>() }
        _list.add(i)
    }
    val _res = mutableListOf<MutableMap<String, Any>>()
val _tmp = mutableListOf<Pair<Any, MutableMap<String, Any>>>()
    for ((key, items) in _groups) {
        val g = GGroup(key, items)
        _tmp.add(Pair(g.key, mutableMapOf("cat" to g.key, "share" to (run {
    val _res = mutableListOf<Any>()
    for (x in g.items) {
        _res.add(if (x["flag"] != null) x["val"] else 0)
    }
    _res
}.map{(it as Number).toDouble()}).sum() / (run {
    val _res = mutableListOf<Any>()
    for (x in g.items) {
        _res.add(x["val"])
    }
    _res
}.map{(it as Number).toDouble()}).sum())))
    }
    _tmp.sortBy { it.first }.map { it.second }.toMutableList().also { _res.addAll(it) }
    _res
}
    println(result)
}
