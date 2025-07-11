fun toInt(v: Any?): Int = when (v) {
    is Int -> v
    is Double -> v.toInt()
    is String -> v.toInt()
    is Boolean -> if (v) 1 else 0
    else -> 0
}

class Group(val key: Any?, val items: MutableList<Any?>) : MutableList<Any?> by items
data class Data(var tag: String, var `val`: Int)

val data = mutableListOf(Data(tag = "a", `val` = 1), Data(tag = "a", `val` = 2), Data(tag = "b", `val` = 3))

val groups = run {
    val __groups = mutableMapOf<Any?, Group>()
    val __order = mutableListOf<Any?>()
    for (d in data) {
        val __k = d.tag
        var __g = __groups[__k]
        if (__g == null) {
            __g = Group(__k, mutableListOf())
            __groups[__k] = __g
            __order.add(__k)
        }
        __g.add(d)
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
            total = total + toInt((x as MutableMap<*, *>)["val"])
        }
        tmp = tmp + mutableMapOf("tag" to g.key, "total" to total)
    }
    println(result)
}
