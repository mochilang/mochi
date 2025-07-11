class Group(val key: Any?, val items: MutableList<Any?>) : MutableList<Any?> by items
data class Filtered(var part: Any?, var value: Any?)

data class Grouped(var part: Any?, var total: Int)

data class Nation(var id: Int, var name: String)

data class Supplier(var id: Int, var nation: Int)

data class Partsupp(var part: Int, var supplier: Int, var cost: Double, var qty: Int)

val nations = mutableListOf(Nation(id = 1, name = "A"), Nation(id = 2, name = "B"))

val suppliers = mutableListOf(Supplier(id = 1, nation = 1), Supplier(id = 2, nation = 2))

val partsupp = mutableListOf(Partsupp(part = 100, supplier = 1, cost = 10, qty = 2), Partsupp(part = 100, supplier = 2, cost = 20, qty = 1), Partsupp(part = 200, supplier = 1, cost = 5, qty = 3))

val filtered = run {
    val __res = mutableListOf<Filtered>()
    for (ps in partsupp) {
        for (s in suppliers) {
            if (s.id == ps.supplier) {
                for (n in nations) {
                    if (n.id == s.nation) {
                        if (n.name == "A") {
                            __res.add(Filtered(part = ps.part, value = ps.cost * ps.qty))
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
        val __k = x.part
        var __g = __groups[__k]
        if (__g == null) {
            __g = Group(__k, mutableListOf())
            __groups[__k] = __g
            __order.add(__k)
        }
        __g.add(x)
    }
    val __res = mutableListOf<Grouped>()
    for (k in __order) {
        val g = __groups[k]!!
        __res.add(Grouped(part = g.key, total = run {
    val __res = mutableListOf<Any?>()
    for (r in g) {
        __res.add(r.value)
    }
    __res
}.sum()))
    }
    __res
}

fun main() {
    println(grouped)
}
