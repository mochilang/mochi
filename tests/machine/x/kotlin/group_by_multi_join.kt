fun sum(list: List<Any?>): Int {
    var s = 0
    for (n in list) s += toInt(n)
    return s
}

fun toBool(v: Any?): Boolean = when (v) {
    is Boolean -> v
    is Int -> v != 0
    is Double -> v != 0.0
    is String -> v.isNotEmpty()
    null -> false
    else -> true
}

class Group(val key: Any?, val items: MutableList<Any?>) : MutableList<Any?> by items
data class Nation(var id: Int, var name: String)

data class Supplier(var id: Int, var nation: Int)

data class Partsupp(var part: Int, var supplier: Int, var cost: Double, var qty: Int)

val nations = mutableListOf(Nation(id = 1, name = "A"), Nation(id = 2, name = "B"))

val suppliers = mutableListOf(Supplier(id = 1, nation = 1), Supplier(id = 2, nation = 2))

val partsupp = mutableListOf(Partsupp(part = 100, supplier = 1, cost = 10, qty = 2), Partsupp(part = 100, supplier = 2, cost = 20, qty = 1), Partsupp(part = 200, supplier = 1, cost = 5, qty = 3))

val filtered = run {
    val __res = mutableListOf<MutableMap<Any?, Any?>>()
    for (ps in partsupp) {
        for (s in suppliers) {
            if (toBool(s.id == ps.supplier)) {
                for (n in nations) {
                    if (toBool(n.id == s.nation)) {
                        if (toBool(n.name == "A")) {
                            __res.add((mutableMapOf("part" to ps.part, "value" to ps.cost * ps.qty) as MutableMap<Any?, Any?>))
                        }
                    }
                }
            }
        }
    }
    __res
}

val grouped = run {
    val __groups = mutableMapOf<Any?, Group>()
    val __order = mutableListOf<Any?>()
    for (x in filtered) {
        val __k = (x as MutableMap<*, *>)["part"]
        var __g = __groups[__k]
        if (__g == null) {
            __g = Group(__k, mutableListOf())
            __groups[__k] = __g
            __order.add(__k)
        }
        __g.add(mutableMapOf("x" to x) as MutableMap<Any?, Any?>)
    }
    val __res = mutableListOf<MutableMap<Any?, Any?>>()
    for (k in __order) {
        val g = __groups[k]!!
        __res.add((mutableMapOf("part" to g.key, "total" to sum(run {
    val __res = mutableListOf<Any?>()
    for (r in g) {
        __res.add((r as MutableMap<*, *>)["value"])
    }
    __res
})) as MutableMap<Any?, Any?>))
    }
    __res
}

fun main() {
    println(grouped)
}
