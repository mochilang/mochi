data class Person(val name: String, val age: Int, val city: String)

data class Stat(val city: String, val count: Int, val avgAge: Double)

fun main() {
    val people = listOf(
        Person("Alice", 30, "Paris"),
        Person("Bob", 15, "Hanoi"),
        Person("Charlie", 65, "Paris"),
        Person("Diana", 45, "Hanoi"),
        Person("Eve", 70, "Paris"),
        Person("Frank", 22, "Hanoi")
    )
    val stats = people.groupBy { it.city }.map { (city, list) ->
        val avgAge = list.map { it.age }.average()
        Stat(city, list.size, avgAge)
    }
    println("--- People grouped by city ---")
    for (s in stats) {
        println("${s.city}: count = ${s.count}, avg_age = ${s.avgAge}")
    }
}
