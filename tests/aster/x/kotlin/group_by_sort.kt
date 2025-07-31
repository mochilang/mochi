data class GGroup(val key: Any, val items: MutableList<MutableMap<String, Any>>)

fun main() {
    val items = mutableListOf(mutableMapOf("cat" to "a", "val" to 3), mutableMapOf("cat" to "a", "val" to 1), mutableMapOf("cat" to "b", "val" to 5), mutableMapOf("cat" to "b", "val" to 2))
    val grouped = run {
        val _groups = mutableMapOftype_arguments
        for (i in items) {
            val _list = _groups.getOrPut(as_expression) {mutableListOftype_arguments}
            _list.add(i)
        }
        val _res = mutableListOftype_arguments
        val _tmp = mutableListOftype_arguments
        for (key, items in _groups) {
            val g = GGroup(key, items)
            _tmp.add(Pair(0 - (run {
                val _res = mutableListOftype_arguments
                for (x in g.items) {
                    _res.add(x["val"])
                }
                _res
            }.map {(as_expression).toDouble()}).sum(), mutableMapOf("cat" to g.key, "total" to (run {
                val _res = mutableListOftype_arguments
                for (x in g.items) {
                    _res.add(x["val"])
                }
                _res
            }.map {(as_expression).toDouble()}).sum())))
        }
        _tmp.sortBy {it.first}.map {it.second}.toMutableList().also {_res.addAll(it)}
        _res
    }
    println(grouped)
}
