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
val people = mutableListOf(mutableMapOf("name" to "Alice", "city" to "Paris"), mutableMapOf("name" to "Bob", "city" to "Hanoi"), mutableMapOf("name" to "Charlie", "city" to "Paris"), mutableMapOf("name" to "Diana", "city" to "Hanoi"), mutableMapOf("name" to "Eve", "city" to "Paris"), mutableMapOf("name" to "Frank", "city" to "Hanoi"), mutableMapOf("name" to "George", "city" to "Paris"))

val big = run {
    val __groups = mutableMapOf<Any?, Group>()
    val __order = mutableListOf<Any?>()
    for (p in people) {
        val __k = (p as MutableMap<*, *>)["city"]
        var __g = __groups[__k]
        if (__g == null) {
            __g = Group(__k, mutableListOf())
            __groups[__k] = __g
            __order.add(__k)
        }
        __g.add(mutableMapOf("p" to p) as MutableMap<Any?, Any?>)
    }
    val __res = mutableListOf<MutableMap<Any?, Any?>>()
    for (k in __order) {
        val g = __groups[k]!!
        if (toBool(count(g) >= 4)) {
            __res.add((mutableMapOf("city" to g.key, "num" to count(g)) as MutableMap<Any?, Any?>))
        }
    }
    __res
}

fun main() {
    json(big)
}
