fun avg(list: List<Any?>): Double {
    if (list.isEmpty()) return 0.0
    var s = 0.0
    for (n in list) s += toDouble(n)
    return s / list.size
}

fun count(list: Collection<Any?>): Int = list.size

class Group(val key: Any?, val items: MutableList<Any?>) : MutableList<Any?> by items
data class People(var name: String, var age: Int, var city: String)

val people = mutableListOf(People(name = "Alice", age = 30, city = "Paris"), People(name = "Bob", age = 15, city = "Hanoi"), People(name = "Charlie", age = 65, city = "Paris"), People(name = "Diana", age = 45, city = "Hanoi"), People(name = "Eve", age = 70, city = "Paris"), People(name = "Frank", age = 22, city = "Hanoi"))

val stats = run {
    val __groups = mutableMapOf<Any?, Group>()
    val __order = mutableListOf<Any?>()
    for (person in people) {
        val __k = person.city
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
    val __res = mutableListOf<Int>()
    for (p in g) {
        __res.add(p.age)
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
