data class GGroup(val key: MutableMap<String, Any>, val items: MutableList<MutableMap<String, MutableMap<String, Any>>>)
fun main() {
    val items: MutableList<MutableMap<String, Any>> = mutableListOf(mutableMapOf<String, Any>("a" to "x", "b" to 1, "val" to 2), mutableMapOf<String, Any>("a" to "x", "b" to 2, "val" to 3), mutableMapOf<String, Any>("a" to "y", "b" to 1, "val" to 4), mutableMapOf<String, Any>("a" to "y", "b" to 2, "val" to 1))
    val grouped: MutableList<MutableMap<String, Any>> = run {
    val _groups = mutableMapOf<MutableMap<String, Any>, MutableList<MutableMap<String, MutableMap<String, Any>>>>()
    for (i in items) {
        val _list = _groups.getOrPut(mutableMapOf<String, Any>("a" to (i["a"]!!), "b" to (i["b"]!!))) { mutableListOf<MutableMap<String, MutableMap<String, Any>>>() }
        _list.add(mutableMapOf<String, MutableMap<String, Any>>("i" to i))
    }
    val _res = mutableListOf<MutableMap<String, Any>>()
val _tmp = mutableListOf<Pair<Double, MutableMap<String, Any>>>()
    for ((key, items) in _groups) {
        val g = GGroup(key, items)
        _tmp.add(Pair(0 - (run {
    var _acc = 0.0
    for (x in g.items) {
        _acc += ((x["val"]!!) as Number).toDouble()
    }
    _acc
} as Number).toDouble(), mutableMapOf<String, Any>("a" to (g.key["a"]!!), "b" to (g.key["b"]!!), "total" to run {
    var _acc = 0.0
    for (x in g.items) {
        _acc += ((x["val"]!!) as Number).toDouble()
    }
    _acc
})))
    }
    _tmp.sortedBy { it.first }.map { it.second }.toMutableList().also { _res.addAll(it) }
    _res
}
    println(grouped)
}
