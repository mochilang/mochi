data class GGroup(val key: MutableMap<String, Any>, val items: MutableList<MutableMap<String, MutableMap<String, Any>>>)

fun main() {
    val nation: MutableList<MutableMap<String, Any>> = mutableListOf(mutableMapOftype_arguments)
    val customer: MutableList<MutableMap<String, Any>> = mutableListOf(mutableMapOftype_arguments)
    val orders: MutableList<MutableMap<String, Any>> = mutableListOf(mutableMapOftype_arguments, mutableMapOftype_arguments)
    val lineitem: MutableList<MutableMap<String, Any>> = mutableListOf(mutableMapOftype_arguments, mutableMapOftype_arguments)
    val start_date: String = "1993-10-01"
    val end_date: String = "1994-01-01"
    val result: MutableList<MutableMap<String, Any>> = run {
        val _groups = mutableMapOftype_arguments
        for (c in customer) {
            for (o in orders) {
                for (l in lineitem) {
                    for (n in nation) {
                        if (((((!o["o_custkey"]) == (!c["c_custkey"])) && ((!l["l_orderkey"]) == (!o["o_orderkey"]))) && ((!n["n_nationkey"]) == (!c["c_nationkey"]))) && (((((!o["o_orderdate"])).toString() >= start_date) && (((!o["o_orderdate"])).toString() < end_date)) && ((!l["l_returnflag"]) == "R"))) {
                            val _list = _groups.getOrPut(mutableMapOftype_arguments) {mutableListOftype_arguments}
                            _list.add(mutableMapOftype_arguments)
                        }
                    }
                }
            }
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
    println(result)
}
