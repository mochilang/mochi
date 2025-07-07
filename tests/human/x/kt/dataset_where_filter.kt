data class Person(val name: String, val age: Int)
data class PersonInfo(val name: String, val age: Int, val isSenior: Boolean)

fun main() {
    val people = listOf(
        Person("Alice", 30),
        Person("Bob", 15),
        Person("Charlie", 65),
        Person("Diana", 45)
    )
    val adults = people.filter { it.age >= 18 }
        .map { PersonInfo(it.name, it.age, it.age >= 60) }
    println("--- Adults ---")
    for (p in adults) {
        val senior = if (p.isSenior) " (senior)" else ""
        println("${p.name} is ${p.age}$senior")
    }
}
