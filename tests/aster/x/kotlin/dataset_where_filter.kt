fun main() {
    val people = mutableListOf(mutableMapOf("name" to "Alice", "age" to 30), mutableMapOf("name" to "Bob", "age" to 15), mutableMapOf("name" to "Charlie", "age" to 65), mutableMapOf("name" to "Diana", "age" to 45))
    val adults = run()
    println("--- Adults ---")
    for (person in adults) {
        println(listOf(person["name"], "is", person["age"], if_expression).joinToString(" "))
    }
}
