data class People(var name: String, var age: Int)

data class Adult(var name: Any?, var age: Any?, var is_senior: Boolean)

val people = mutableListOf(People(name = "Alice", age = 30), People(name = "Bob", age = 15), People(name = "Charlie", age = 65), People(name = "Diana", age = 45))

val adults = run {
    val __res = mutableListOf<Adult>()
    for (person in people) {
        if (person.age >= 18) {
            __res.add(Adult(name = person.name, age = person.age, is_senior = person.age >= 60))
        }
    }
    __res
}

fun main() {
    println("--- Adults ---")
    for (person in adults) {
        println(listOf(person.name, "is", person.age, if (person.is_senior) " (senior)" else "").joinToString(" "))
    }
}
