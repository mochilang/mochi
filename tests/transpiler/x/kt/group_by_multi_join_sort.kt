data class GGroup(val key: MutableMap<String, Any>, val items: MutableList<MutableMap<String, MutableMap<String, Any>>>)
fun main() {
    val nation: MutableList<MutableMap<String, Any>> = mutableListOf(mutableMapOf<String, Any>("n_nationkey" to 1, "n_name" to "BRAZIL"))
    val customer: MutableList<MutableMap<String, Any>> = mutableListOf(mutableMapOf<String, Any>("c_custkey" to 1, "c_name" to "Alice", "c_acctbal" to 100.0, "c_nationkey" to 1, "c_address" to "123 St", "c_phone" to "123-456", "c_comment" to "Loyal"))
    val orders: MutableList<MutableMap<String, Any>> = mutableListOf(mutableMapOf<String, Any>("o_orderkey" to 1000, "o_custkey" to 1, "o_orderdate" to "1993-10-15"), mutableMapOf<String, Any>("o_orderkey" to 2000, "o_custkey" to 1, "o_orderdate" to "1994-01-02"))
    val lineitem: MutableList<MutableMap<String, Any>> = mutableListOf(mutableMapOf<String, Any>("l_orderkey" to 1000, "l_returnflag" to "R", "l_extendedprice" to 1000.0, "l_discount" to 0.1), mutableMapOf<String, Any>("l_orderkey" to 2000, "l_returnflag" to "N", "l_extendedprice" to 500.0, "l_discount" to 0.0))
    val start_date: String = "1993-10-01"
    val end_date: String = "1994-01-01"
    val result: MutableList<MutableMap<String, Any>> = run {
    val _groups = mutableMapOf<MutableMap<String, Any>, MutableList<MutableMap<String, MutableMap<String, Any>>>>()
    for (c in customer) {
        for (o in orders) {
            for (l in lineitem) {
                for (n in nation) {
                    if (((((o["o_custkey"]!!) == (c["c_custkey"]!!)) && ((l["l_orderkey"]!!) == (o["o_orderkey"]!!))) && ((n["n_nationkey"]!!) == (c["c_nationkey"]!!))) && (((((o["o_orderdate"]!!)).toString() >= start_date) && (((o["o_orderdate"]!!)).toString() < end_date)) && ((l["l_returnflag"]!!) == "R"))) {
                    val _list = _groups.getOrPut(mutableMapOf<String, Any>("c_custkey" to (c["c_custkey"]!!), "c_name" to (c["c_name"]!!), "c_acctbal" to (c["c_acctbal"]!!), "c_address" to (c["c_address"]!!), "c_phone" to (c["c_phone"]!!), "c_comment" to (c["c_comment"]!!), "n_name" to (n["n_name"]!!))) { mutableListOf<MutableMap<String, MutableMap<String, Any>>>() }
                    _list.add(mutableMapOf<String, MutableMap<String, Any>>("c" to c, "o" to o, "l" to l, "n" to n))
                    }
                }
            }
        }
    }
    val _res = mutableListOf<MutableMap<String, Any>>()
val _tmp = mutableListOf<Pair<Double, MutableMap<String, Any>>>()
    for ((key, items) in _groups) {
        val g = GGroup(key, items)
        _tmp.add(Pair(0 - (run {
    val _res = mutableListOf<Double>()
    for (x in g.items) {
        _res.add((((x["l"] as MutableMap<String, Any>)["l_extendedprice"]!!) as Number).toDouble() * (1 - (((x["l"] as MutableMap<String, Any>)["l_discount"]!!) as Number).toDouble()))
    }
    _res
}.sum() as Number).toDouble(), mutableMapOf<String, Any>("c_custkey" to (g.key["c_custkey"]!!), "c_name" to (g.key["c_name"]!!), "revenue" to run {
    val _res = mutableListOf<Double>()
    for (x in g.items) {
        _res.add((((x["l"] as MutableMap<String, Any>)["l_extendedprice"]!!) as Number).toDouble() * (1 - (((x["l"] as MutableMap<String, Any>)["l_discount"]!!) as Number).toDouble()))
    }
    _res
}.sum(), "c_acctbal" to (g.key["c_acctbal"]!!), "n_name" to (g.key["n_name"]!!), "c_address" to (g.key["c_address"]!!), "c_phone" to (g.key["c_phone"]!!), "c_comment" to (g.key["c_comment"]!!))))
    }
    _tmp.sortedBy { it.first }.map { it.second }.toMutableList().also { _res.addAll(it) }
    _res
}
    println(result)
}
