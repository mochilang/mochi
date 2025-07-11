class Group(val key: Any?, val items: MutableList<Any?>) : MutableList<Any?> by items
data class Grouped(var cat: Any?, var total: Int)

data class Item(var cat: String, var `val`: Int)

val items = mutableListOf(Item(cat = "a", `val` = 3), Item(cat = "a", `val` = 1), Item(cat = "b", `val` = 5), Item(cat = "b", `val` = 2))

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
        __g.add(i)
    }
    val __res = mutableListOf<Grouped>()
    for (k in __order) {
        val g = __groups[k]!!
        __res.add(Grouped(cat = g.key, total = run {
    val __res = mutableListOf<Int>()
    for (x in g) {
        __res.add(x.val)
    }
    __res
}.sum()))
    }
    __res
}.sortedByDescending { run {
    val __res = mutableListOf<Int>()
    for (x in it) {
        __res.add(x.val)
    }
    __res
}.sum() as Comparable<Any> }

fun main() {
    println(grouped)
}
