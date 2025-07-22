fun expect(cond: Boolean) { if (!cond) throw RuntimeException("expect failed") }

data class Person(val name: String, val age: Int, val status: String)
fun test_update_adult_status(): Unit {
    expect(people == mutableListOf(Person(name = "Alice", age = 17, status = "minor"), Person(name = "Bob", age = 26, status = "adult"), Person(name = "Charlie", age = 19, status = "adult"), Person(name = "Diana", age = 16, status = "minor")))
}

fun main() {
    val people: MutableList<Person> = mutableListOf(Person(name = "Alice", age = 17, status = "minor"), Person(name = "Bob", age = 25, status = "unknown"), Person(name = "Charlie", age = 18, status = "unknown"), Person(name = "Diana", age = 16, status = "minor"))
    for (_i6 in 0 until people.size) {
        var _it6: Person = (people[_i6] as Person)
        val name: String = _it6.name
        val age: Int = _it6.age
        val status: String = _it6.status
        if (age >= 18) {
            _it6.status = "adult"
            _it6.age = age + 1
        }
        (people[_i6] as Person) = _it6
    }
    test_update_adult_status()
    println("ok")
}
