fun count(list: Collection<Any?>): Int = list.size

fun toBool(v: Any?): Boolean = when (v) {
    is Boolean -> v
    is Int -> v != 0
    is Double -> v != 0.0
    is String -> v.isNotEmpty()
    null -> false
    else -> true
}

fun json(v: Any?) {
    println(toJson(v))
}

fun toJson(v: Any?): String = when (v) {
    null -> "null"
    is String -> "\"" + v.replace("\"", "\\\"") + "\""
    is Boolean, is Number -> v.toString()
    is Map<*, *> -> v.entries.joinToString(prefix = "{", postfix = "}") { toJson(it.key.toString()) + ":" + toJson(it.value) }
    is Iterable<*> -> v.joinToString(prefix = "[", postfix = "]") { toJson(it) }
    else -> toJson(v.toString())
}

class Group(val key: Any?, val items: MutableList<Any?>) : MutableList<Any?> by items
data class People(var name: String, var city: String)

data class Big(var city: Any?, var num: Int)

val people = mutableListOf(People(name = "Alice", city = "Paris"), People(name = "Bob", city = "Hanoi"), People(name = "Charlie", city = "Paris"), People(name = "Diana", city = "Hanoi"), People(name = "Eve", city = "Paris"), People(name = "Frank", city = "Hanoi"), People(name = "George", city = "Paris"))

val big = run {
    val __groups = mutableMapOf<Any?, Group>()
    val __order = mutableListOf<Any?>()
    for (p in people) {
        val __k = p.city
        var __g = __groups[__k]
        if (__g == null) {
            __g = Group(__k, mutableListOf())
            __groups[__k] = __g
            __order.add(__k)
        }
        __g.add(mutableMapOf("p" to p) as MutableMap<Any?, Any?>)
    }
    val __res = mutableListOf<Big>()
    for (k in __order) {
        val g = __groups[k]!!
        if (toBool(count(g) >= 4)) {
            __res.add(Big(city = g.key, num = count(g)))
        }
    }
    __res
}

fun main() {
    json(big)
}
