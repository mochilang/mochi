fun toBool(v: Any?): Boolean = when (v) {
    is Boolean -> v
    is Int -> v != 0
    is Double -> v != 0.0
    is String -> v.isNotEmpty()
    null -> false
    else -> true
}
data class People(var name: String, var age: Int)

val people = mutableListOf(People(name = "Alice", age = 30), People(name = "Bob", age = 15), People(name = "Charlie", age = 65), People(name = "Diana", age = 45))

val adults = run {
    val __res = mutableListOf<MutableMap<Any?, Any?>>()
    for (person in people) {
        if (toBool(person.age >= 18)) {
            __res.add((mutableMapOf("name" to person.name, "age" to person.age, "is_senior" to person.age >= 60) as MutableMap<Any?, Any?>))
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
