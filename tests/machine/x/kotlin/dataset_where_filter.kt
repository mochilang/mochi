fun toInt(v: Any?): Int = when (v) {
    is Int -> v
    is Double -> v.toInt()
    is String -> v.toInt()
    is Boolean -> if (v) 1 else 0
    else -> 0
}

fun toBool(v: Any?): Boolean = when (v) {
    is Boolean -> v
    is Int -> v != 0
    is Double -> v != 0.0
    is String -> v.isNotEmpty()
    null -> false
    else -> true
}
val people = mutableListOf(mutableMapOf("name" to "Alice", "age" to 30), mutableMapOf("name" to "Bob", "age" to 15), mutableMapOf("name" to "Charlie", "age" to 65), mutableMapOf("name" to "Diana", "age" to 45))

val adults = run {
    val __res = mutableListOf<MutableMap<Any?, Any?>>()
    for (person in people) {
        if (toBool(toInt((person as MutableMap<*, *>)["age"]) >= 18)) {
            __res.add((mutableMapOf("name" to (person as MutableMap<*, *>)["name"], "age" to (person as MutableMap<*, *>)["age"], "is_senior" to toInt((person as MutableMap<*, *>)["age"]) >= 60) as MutableMap<Any?, Any?>))
        }
    }
    __res
}

fun main() {
    println("--- Adults ---")
    for (person in adults) {
        println(listOf((person as MutableMap<*, *>)["name"], "is", (person as MutableMap<*, *>)["age"], if (toBool((person as MutableMap<*, *>)["is_senior"])) " (senior)" else "").joinToString(" "))
    }
}
