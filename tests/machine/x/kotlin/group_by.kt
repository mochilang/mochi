fun toDouble(v: Any?): Double = when (v) {
    is Double -> v
    is Int -> v.toDouble()
    is String -> v.toDouble()
    else -> 0.0
}

class Group<K, T>(val key: K, val items: MutableList<T>) : MutableList<T> by items
// Code generated from tests/vm/valid/group_by.mochi

data class People(var name: String, var age: Int, var city: String)

data class Stat(var city: Any?, var count: Int, var avg_age: Double)

val people = mutableListOf(People(name = "Alice", age = 30, city = "Paris"), People(name = "Bob", age = 15, city = "Hanoi"), People(name = "Charlie", age = 65, city = "Paris"), People(name = "Diana", age = 45, city = "Hanoi"), People(name = "Eve", age = 70, city = "Paris"), People(name = "Frank", age = 22, city = "Hanoi"))

val stats = run {
    val __groups = mutableMapOf<Any?, Group<Any?, People>>()
    val __order = mutableListOf<Any?>()
    for (person in people) {
        val __k = person.city
        var __g = __groups[__k]
        if (__g == null) {
            __g = Group(__k, mutableListOf<People>())
            __groups[__k] = __g
            __order.add(__k)
        }
        __g.add(person)
    }
    val __res = mutableListOf<Stat>()
    for (k in __order) {
        val g = __groups[k]!!
        __res.add(Stat(city = g.key, count = g.size, avg_age = run {
    val __res = mutableListOf<Int>()
    for (p in g) {
        __res.add(p.age)
    }
    __res
}.map{ toDouble(it) }.average()))
    }
    __res
}

fun main() {
    println("--- People grouped by city ---")
    for (s in stats) {
        println(listOf(s.city, ": count =", s.count, ", avg_age =", s.avg_age).joinToString(" "))
    }
}
