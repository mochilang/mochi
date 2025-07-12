fun toInt(v: Any?): Int = when (v) {
    is Int -> v
    is Double -> v.toInt()
    is String -> v.toInt()
    is Boolean -> if (v) 1 else 0
    else -> 0
}

class Group<K, T>(val key: K, val items: MutableList<T>) : MutableList<T> by items
// Code generated from tests/vm/valid/group_items_iteration.mochi

data class Data(var tag: String, var `val`: Int)

val data = mutableListOf(Data(tag = "a", `val` = 1), Data(tag = "a", `val` = 2), Data(tag = "b", `val` = 3))

val groups = run {
    val __groups = mutableMapOf<Any?, Group<Any?, Data>>()
    val __order = mutableListOf<Any?>()
    for (d in data) {
        val __k = d.tag
        var __g = __groups[__k]
        if (__g == null) {
            __g = Group(__k, mutableListOf<Data>())
            __groups[__k] = __g
            __order.add(__k)
        }
        __g.add(d)
    }
    val __res = mutableListOf<Group<Any?, Data>>()
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
            total = total + toInt((x as MutableMap<*, *>)["val"])
        }
        tmp = tmp + mutableMapOf("tag" to g.key, "total" to total)
    }
    println(result)
}
