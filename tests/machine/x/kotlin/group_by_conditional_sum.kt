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

class Group<K, T>(val key: K, val items: MutableList<T>) : MutableList<T> by items
// Code generated from tests/vm/valid/group_by_conditional_sum.mochi

data class Item(var cat: String, var `val`: Int, var flag: Boolean)

data class Result(var cat: Any?, var share: Double)

val items = mutableListOf(Item(cat = "a", `val` = 10, flag = true), Item(cat = "a", `val` = 5, flag = false), Item(cat = "b", `val` = 20, flag = true))

val result = run {
    val __groups = mutableMapOf<Any?, Group<Any?, Item>>()
    val __order = mutableListOf<Any?>()
    for (i in items) {
        val __k = i.cat
        var __g = __groups[__k]
        if (__g == null) {
            __g = Group(__k, mutableListOf<Item>())
            __groups[__k] = __g
            __order.add(__k)
        }
        __g.add(i)
    }
    val __res = mutableListOf<Result>()
    for (k in __order) {
        val g = __groups[k]!!
        __res.add(Result(cat = g.key, share = sum(run {
    val __res = mutableListOf<Any?>()
    for (x in g) {
        __res.add(if (x.flag) x.`val` else 0)
    }
    __res
}) / sum(run {
    val __res = mutableListOf<Int>()
    for (x in g) {
        __res.add(x.`val`)
    }
    __res
})))
    }
    __res
}.sortedBy { it.key as Comparable<Any> }

fun main() {
    println(result)
}
