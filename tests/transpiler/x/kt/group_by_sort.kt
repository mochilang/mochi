data class GGroup(val key: Any, val items: MutableList<MutableMap<String, Any>>)
fun main() {
    val items = mutableListOf(mutableMapOf("cat" to "a", "val" to 3) as MutableMap<String, Any>, mutableMapOf("cat" to "a", "val" to 1) as MutableMap<String, Any>, mutableMapOf("cat" to "b", "val" to 5) as MutableMap<String, Any>, mutableMapOf("cat" to "b", "val" to 2) as MutableMap<String, Any>)
    val grouped = run {
    val _groups = mutableMapOf<Any, MutableList<MutableMap<String, Any>>>()
    for (i in items) {
        val _list = _groups.getOrPut(i["cat"]!! as Any) { mutableListOf<MutableMap<String, Any>>() }
        _list.add(i)
    }
    val _res = mutableListOf<MutableMap<String, Any>>()
val _tmp = mutableListOf<Pair<Any, MutableMap<String, Any>>>()
    for ((key, items) in _groups) {
        val g = GGroup(key, items)
        _tmp.add(Pair(0 - (run {
    val _res = mutableListOf<Any>()
    for (x in g.items) {
        _res.add(x["val"]!!)
    }
    _res
}.map{(it as Number).toDouble()}).sum(), mutableMapOf("cat" to g.key, "total" to (run {
    val _res = mutableListOf<Any>()
    for (x in g.items) {
        _res.add(x["val"]!!)
    }
    _res
}.map{(it as Number).toDouble()}).sum()) as MutableMap<String, Any>))
    }
    _tmp.sortBy { it.first }.map { it.second }.toMutableList().also { _res.addAll(it) }
    _res
}
    println(grouped)
}
