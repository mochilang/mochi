data class Person(val name: String, val age: Int, val status: String)

val people: Any = mutableListOf(Person(name = "Alice", age = 17, status = "minor"), Person(name = "Bob", age = 25, status = "unknown"), Person(name = "Charlie", age = 18, status = "unknown"), Person(name = "Diana", age = 16, status = "minor"))

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
    check(people == mutableListOf(Person(name = "Alice", age = 17, status = "minor"), Person(name = "Bob", age = 26, status = "adult"), Person(name = "Charlie", age = 19, status = "adult"), Person(name = "Diana", age = 16, status = "minor")))
    println("ok")
}
