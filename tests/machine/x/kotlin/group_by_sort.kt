fun sum(list: List<Any?>): Int {
    var s = 0
    for (n in list) s += toInt(n)
    return s
}

class Group(val key: Any?, val items: MutableList<Any?>) : MutableList<Any?> by items
val items = mutableListOf(mutableMapOf("cat" to "a", "val" to 3), mutableMapOf("cat" to "a", "val" to 1), mutableMapOf("cat" to "b", "val" to 5), mutableMapOf("cat" to "b", "val" to 2))

val grouped = run {
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
        __res.add((mutableMapOf("cat" to g.key, "total" to sum(run {
    val __res = mutableListOf<Any?>()
    for (x in g) {
        __res.add((x as MutableMap<*, *>)["val"])
    }
    __res
})) as MutableMap<Any?, Any?>))
    }
    __res
}.sortedByDescending { sum(run {
    val __res = mutableListOf<Any?>()
    for (x in it) {
        __res.add((x as MutableMap<*, *>)["val"])
    }
    __res
}) as Comparable<Any> }

fun main() {
    println(grouped)
}
