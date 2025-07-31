data class GGroup(val key: MutableMap<String, Any>, val items: MutableList<MutableMap<String, MutableMap<String, Any>>>)

fun main() {
    val items: MutableList<MutableMap<String, Any>> = mutableListOf(mutableMapOftype_arguments, mutableMapOftype_arguments, mutableMapOftype_arguments, mutableMapOftype_arguments)
    val grouped: MutableList<MutableMap<String, Any>> = run {
        val _groups = mutableMapOftype_arguments
        for (i in items) {
            val _list = _groups.getOrPut(mutableMapOftype_arguments) {mutableListOftype_arguments}
            _list.add(mutableMapOftype_arguments)
        }
        val _res = mutableListOftype_arguments
        val _tmp = mutableListOftype_arguments
        for (key, items in _groups) {
            val g = GGroup(key, items)
            _tmp.add(Pair(0 - (as_expression).toDouble(), mutableMapOftype_arguments))
        }
        _tmp.sortedBy {it.first}.map {it.second}.toMutableList().also {_res.addAll(it)}
        _res
    }
    println(grouped)
}
