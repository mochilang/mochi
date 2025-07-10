fun avg(list: List<Any?>): Double {
    if (list.isEmpty()) return 0.0
    var s = 0.0
    for (n in list) s += toDouble(n)
    return s / list.size
}

fun count(list: Collection<Any?>): Int = list.size

class Group(val key: Any?, val items: MutableList<Any?>) : MutableList<Any?> by items
val people = mutableListOf(mutableMapOf("name" to "Alice", "age" to 30, "city" to "Paris"), mutableMapOf("name" to "Bob", "age" to 15, "city" to "Hanoi"), mutableMapOf("name" to "Charlie", "age" to 65, "city" to "Paris"), mutableMapOf("name" to "Diana", "age" to 45, "city" to "Hanoi"), mutableMapOf("name" to "Eve", "age" to 70, "city" to "Paris"), mutableMapOf("name" to "Frank", "age" to 22, "city" to "Hanoi"))

val stats = run {
    val __groups = mutableMapOf<Any?, Group>()
    val __order = mutableListOf<Any?>()
    for (person in people) {
        val __k = (person as MutableMap<*, *>)["city"]
        var __g = __groups[__k]
        if (__g == null) {
            __g = Group(__k, mutableListOf())
            __groups[__k] = __g
            __order.add(__k)
        }
        __g.add(mutableMapOf("person" to person) as MutableMap<Any?, Any?>)
    }
    val __res = mutableListOf<MutableMap<Any?, Any?>>()
    for (k in __order) {
        val g = __groups[k]!!
        __res.add((mutableMapOf("city" to g.key, "count" to count(g), "avg_age" to avg(run {
    val __res = mutableListOf<Any?>()
    for (p in g) {
        __res.add((p as MutableMap<*, *>)["age"])
    }
    __res
})) as MutableMap<Any?, Any?>))
    }
    __res
}

fun main() {
    println("--- People grouped by city ---")
    for (s in stats) {
        println(listOf((s as MutableMap<*, *>)["city"], ": count =", (s as MutableMap<*, *>)["count"], ", avg_age =", (s as MutableMap<*, *>)["avg_age"]).joinToString(" "))
    }
}
