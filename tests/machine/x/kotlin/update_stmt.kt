// Generated by Mochi compiler v0.10.27 on 2025-07-17T07:07:40Z
fun String.starts_with(prefix: String): Boolean = this.startsWith(prefix)
// Code generated from update_stmt.mochi

data class Person(var name: String, var age: Int, var status: String)

val people: MutableList<Person> = mutableListOf(Person(name = "Alice", age = 17, status = "minor"), Person(name = "Bob", age = 25, status = "unknown"), Person(name = "Charlie", age = 18, status = "unknown"), Person(name = "Diana", age = 16, status = "minor"))

fun main() {
    for (_i0 in 0 until people.size) {
        var _it0 = people[_i0]
        val name = _it0.name
        val age = _it0.age
        val status = _it0.status
        if (age >= 18) {
            _it0.status = "adult"
            _it0.age = age + 1
        }
        people[_i0] = _it0
    }
    println("ok")
}
