fun sum(list: List<Any?>): Int {
    var s = 0
    for (n in list) s += toInt(n)
    return s
}

fun toInt(v: Any?): Int = when (v) {
    is Int -> v
    is Double -> v.toInt()
    is String -> v.toInt()
    is Boolean -> if (v) 1 else 0
    else -> 0
}

fun toDouble(v: Any?): Double = when (v) {
    is Double -> v
    is Int -> v.toDouble()
    is String -> v.toDouble()
    else -> 0.0
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
val nations = mutableListOf(mutableMapOf("id" to 1, "name" to "A"), mutableMapOf("id" to 2, "name" to "B"))

val suppliers = mutableListOf(mutableMapOf("id" to 1, "nation" to 1), mutableMapOf("id" to 2, "nation" to 2))

val partsupp = mutableListOf(mutableMapOf("part" to 100, "supplier" to 1, "cost" to 10, "qty" to 2), mutableMapOf("part" to 100, "supplier" to 2, "cost" to 20, "qty" to 1), mutableMapOf("part" to 200, "supplier" to 1, "cost" to 5, "qty" to 3))

val filtered = run {
    val __res = mutableListOf<MutableMap<Any?, Any?>>()
    for (ps in partsupp) {
        for (s in suppliers) {
            if (toBool((s as MutableMap<*, *>)["id"] == (ps as MutableMap<*, *>)["supplier"])) {
                for (n in nations) {
                    if (toBool((n as MutableMap<*, *>)["id"] == (s as MutableMap<*, *>)["nation"])) {
                        if (toBool((n as MutableMap<*, *>)["name"] == "A")) {
                            __res.add((mutableMapOf("part" to (ps as MutableMap<*, *>)["part"], "value" to toDouble((ps as MutableMap<*, *>)["cost"]) * toDouble((ps as MutableMap<*, *>)["qty"])) as MutableMap<Any?, Any?>))
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
