fun sum(list: List<Any?>): Int {
    var s = 0
    for (n in list) s += toInt(n)
    return s
}

class Group(val key: Any?, val items: MutableList<Any?>) : MutableList<Any?> by items
data class Item(var cat: String, var val: Int)

val items = mutableListOf(Item(cat = "a", val = 3), Item(cat = "a", val = 1), Item(cat = "b", val = 5), Item(cat = "b", val = 2))

val grouped = run {
    val __groups = mutableMapOf<Any?, Group>()
    val __order = mutableListOf<Any?>()
    for (i in items) {
        val __k = i.cat
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
    val __res = mutableListOf<Int>()
    for (x in g) {
        __res.add(x.val)
    }
    __res
})) as MutableMap<Any?, Any?>))
    }
    __res
}.sortedByDescending { sum(run {
    val __res = mutableListOf<Int>()
    for (x in it) {
        __res.add(x.val)
    }
    __res
}) as Comparable<Any> }

fun main() {
    println(grouped)
}
