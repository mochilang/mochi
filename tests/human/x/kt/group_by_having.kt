data class Person(val name: String, val city: String)

data class Group(val city: String, val num: Int)

fun main() {
    val people = listOf(
        Person("Alice", "Paris"),
        Person("Bob", "Hanoi"),
        Person("Charlie", "Paris"),
        Person("Diana", "Hanoi"),
        Person("Eve", "Paris"),
        Person("Frank", "Hanoi"),
        Person("George", "Paris")
    )
    val big = people.groupBy { it.city }
        .filter { (_, list) -> list.size >= 4 }
        .map { (city, list) -> Group(city, list.size) }
    // print as json-like
    println(big)
}
