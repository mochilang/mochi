// Generated by Mochi compiler v0.10.28 on 2025-07-18T07:11:04Z
data class Data(var tag: String, var `val`: Int)

fun <T> append(list: MutableList<T>, item: T): MutableList<T> {
    val res = list.toMutableList()
    res.add(item)
    return res
}

fun toInt(v: Any?): Int = when (v) {
    is Int -> v
    is Double -> v.toInt()
    is String -> v.toInt()
    is Boolean -> if (v) 1 else 0
    else -> 0
}

class Group<K, T>(val key: K, val items: MutableList<T>) : MutableList<T> by items
// Code generated from group_items_iteration.mochi

val data = mutableListOf(Data(tag = "a", `val` = 1), Data(tag = "a", `val` = 2), Data(tag = "b", `val` = 3))

val groups = run {
    val __groups = mutableMapOf<String, Group<String, Data>>()
    val __order = mutableListOf<String>()
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
    val __res = mutableListOf<Group<String, Data>>()
    for (k in __order) {
        val g = __groups[k]!!
        __res.add(g)
    }
    __res
}

var tmp = mutableListOf<Any?>()

fun main() {
    for (g in groups) {
        var total = 0
        for (x in g.items) {
            total = total + toInt((x as MutableMap<String, Any?>)["val"])
        }
        tmp = append(tmp, mutableMapOf("tag" to (g.key), "total" to total))
    }
    val result = run {
    val __sorted = tmp.sortedBy { (it as MutableMap<*, *>)["tag"] as Comparable<Any> }
    val __res = mutableListOf<Any?>()
    for (r in __sorted) {
        __res.add(r)
    }
    __res
}
    println(result)
}
