data class GGroup(val key: Any, val items: MutableList<MutableMap<String, Any>>)

fun main() {
    val nations: MutableList<MutableMap<String, Any>> = mutableListOf(mutableMapOftype_arguments, mutableMapOftype_arguments)
    val suppliers: MutableList<MutableMap<String, Int>> = mutableListOf(mutableMapOftype_arguments, mutableMapOftype_arguments)
    val partsupp: MutableList<MutableMap<String, Any>> = mutableListOf(mutableMapOftype_arguments, mutableMapOftype_arguments, mutableMapOftype_arguments)
    val filtered: MutableList<MutableMap<String, Any>> = run {
        val _res = mutableListOftype_arguments
        for (ps in partsupp) {
            for (s in suppliers) {
                for (n in nations) {
                    if (((!s["id"] == !ps["supplier"]) && (!n["id"] == !s["nation"])) && (!n["name"] == "A")) {
                        _res.add(mutableMapOftype_arguments)
                    }
                }
            }
        }
        _res
    }
    val grouped: MutableList<MutableMap<String, Any>> = run {
        val _groups = mutableMapOftype_arguments
        for (x in filtered) {
            val _list = _groups.getOrPut(as_expression) {mutableListOftype_arguments}
            _list.add(x)
        }
        val _res = mutableListOftype_arguments
        for (key, items in _groups) {
            val g = GGroup(key, items)
            _res.add(mutableMapOftype_arguments)
        }
        _res
    }
    println(grouped)
}
