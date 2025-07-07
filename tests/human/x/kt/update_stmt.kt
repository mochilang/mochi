data class Person(var name: String, var age: Int, var status: String)

fun main() {
    val people = mutableListOf(
        Person("Alice", 17, "minor"),
        Person("Bob", 25, "unknown"),
        Person("Charlie", 18, "unknown"),
        Person("Diana", 16, "minor")
    )
    for (p in people.filter { it.age >= 18 }) {
        p.status = "adult"
        p.age = p.age + 1
    }
    val expected = listOf(
        Person("Alice", 17, "minor"),
        Person("Bob", 26, "adult"),
        Person("Charlie", 19, "adult"),
        Person("Diana", 16, "minor")
    )
    val ok = people.zip(expected).all { (a, b) -> a == b }
    if (!ok) throw RuntimeException("update failed")
    println("ok")
}
