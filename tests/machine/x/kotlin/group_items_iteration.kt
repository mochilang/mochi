fun <T> append(list: MutableList<T>, item: T): MutableList<T> {
    val res = list.toMutableList()
    res.add(item)
    return res
}

fun toDouble(v: Any?): Double = when (v) {
    is Double -> v
    is Int -> v.toDouble()
    is String -> v.toDouble()
    else -> 0.0
}

class Group(val key: Any?, val items: MutableList<Any?>) : MutableList<Any?> by items
val data = mutableListOf(mutableMapOf("tag" to "a", "val" to 1), mutableMapOf("tag" to "a", "val" to 2), mutableMapOf("tag" to "b", "val" to 3))

val groups = run {
    val __groups = mutableMapOf<Any?, Group>()
    val __order = mutableListOf<Any?>()
    for (d in data) {
        val __k = (d as MutableMap<*, *>)["tag"]
        var __g = __groups[__k]
        if (__g == null) {
            __g = Group(__k, mutableListOf())
            __groups[__k] = __g
            __order.add(__k)
        }
        __g.add(mutableMapOf("d" to d) as MutableMap<Any?, Any?>)
    }
    val __res = mutableListOf<Group>()
    for (k in __order) {
        val g = __groups[k]!!
        __res.add(g)
    }
    __res
}

var tmp = mutableListOf()

val result = run {
    val __res = mutableListOf<Any?>()
    for (r in tmp) {
        __res.add(r)
    }
    __res
}.sortedBy { (it as MutableMap<*, *>)["tag"] as Comparable<Any> }

fun main() {
    for (g in groups) {
        var total = 0
        for (x in g.items) {
            total = toDouble(total) + toDouble((x as MutableMap<*, *>)["val"])
        }
        tmp = append(tmp, mutableMapOf("tag" to g.key, "total" to total))
    }
    println(result)
}
