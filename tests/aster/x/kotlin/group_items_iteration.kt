data class GGroup(val key: Any, val items: MutableList<MutableMap<String, MutableMap<String, Any>>>)

fun main() {
    val data: MutableList<MutableMap<String, Any>> = mutableListOf(mutableMapOftype_arguments, mutableMapOftype_arguments, mutableMapOftype_arguments)
    val groups: MutableList<Any> = run {
        val _groups = mutableMapOftype_arguments
        for (d in data) {
            val _list = _groups.getOrPut((!d["tag"])) {mutableListOftype_arguments}
            _list.add(mutableMapOftype_arguments)
        }
        val _res = mutableListOftype_arguments
        for (key, items in _groups) {
            val g = GGroup(key, items)
            _res.add(g)
        }
        _res
    }
    val tmp: MutableList<Any> = mutableListOf()
    for (g in groups) {
        val total = 0
        for (x in g.items) {
            total = (as_expression).toDouble() + (as_expression).toDouble()
        }
        tmp = tmp + mutableMapOftype_arguments
    }
    val result: MutableList<Any> = run {
        val _tmp = mutableListOftype_arguments
        for (r in tmp) {
            _tmp.add(Pair(r.tag, r))
        }
        val _res = _tmp.sortedBy {it.first}.map {it.second}.toMutableList()
        _res
    }
    println(result)
}
