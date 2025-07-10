fun sum(list: List<Any?>): Int {
    var s = 0
    for (n in list) s += toInt(n)
    return s
}

class Group(val key: Any?, val items: MutableList<Any?>) : MutableList<Any?> by items
val items = mutableListOf(mutableMapOf("cat" to "a", "val" to 10, "flag" to true), mutableMapOf("cat" to "a", "val" to 5, "flag" to false), mutableMapOf("cat" to "b", "val" to 20, "flag" to true))

val result = run {
    val __groups = mutableMapOf<Any?, Group>()
    val __order = mutableListOf<Any?>()
    for (i in items) {
        val __k = (i as MutableMap<*, *>)["cat"]
        var __g = __groups[__k]
        if (__g == null) {
            __g = Group(__k, mutableListOf())
            __groups[__k] = __g
            __order.add(__k)
        }
        __g.add(mutableMapOf("i" to i) as MutableMap<Any?, Any?>)
    }
    val __res = mutableListOf<MutableMap<Any?, Any?>>()
    for (k in __order) {
        val g = __groups[k]!!
        __res.add((mutableMapOf("cat" to g.key, "share" to sum(run {
    val __res = mutableListOf<Any?>()
    for (x in g) {
        __res.add(if (toBool((x as MutableMap<*, *>)["flag"])) (x as MutableMap<*, *>)["val"] else 0)
    }
    __res
}) / sum(run {
    val __res = mutableListOf<Any?>()
    for (x in g) {
        __res.add((x as MutableMap<*, *>)["val"])
    }
    __res
})) as MutableMap<Any?, Any?>))
    }
    __res
}.sortedBy { it.key as Comparable<Any> }

fun main() {
    println(result)
}
