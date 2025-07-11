class Group(val key: Any?, val items: MutableList<Any?>) : MutableList<Any?> by items
data class Item(var cat: String, var `val`: Int, var flag: Boolean)

data class Result(var cat: Any?, var share: Double)

val items = mutableListOf(Item(cat = "a", `val` = 10, flag = true), Item(cat = "a", `val` = 5, flag = false), Item(cat = "b", `val` = 20, flag = true))

val result = run {
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
    val __res = mutableListOf<Result>()
    for (k in __order) {
        val g = __groups[k]!!
        __res.add(Result(cat = g.key, share = run {
    val __res = mutableListOf<Any?>()
    for (x in g) {
        __res.add(if (x.flag) x.val else 0)
    }
    __res
}.sum() / run {
    val __res = mutableListOf<Int>()
    for (x in g) {
        __res.add(x.val)
    }
    __res
}.sum()))
    }
    __res
}.sortedBy { it.key as Comparable<Any> }

fun main() {
    println(result)
}
