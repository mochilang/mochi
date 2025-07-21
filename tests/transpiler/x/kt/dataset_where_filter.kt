fun main() {
    val people = mutableListOf(mutableMapOf("name" to "Alice", "age" to 30), mutableMapOf("name" to "Bob", "age" to 15), mutableMapOf("name" to "Charlie", "age" to 65), mutableMapOf("name" to "Diana", "age" to 45))
    val adults = run {
    val _res = mutableListOf<MutableMap<String, Any>>()
    for (person in people) {
        if (person["age"] >= 18) {
            _res.add(mutableMapOf("name" to person["name"], "age" to person["age"], "is_senior" to person["age"] >= 60))
        }
    }
    _res
}
    println("--- Adults ---")
    for (person in adults) {
        println(listOf(person["name"], "is", person["age"], if (person["is_senior"] != null) " (senior)" else "").joinToString(" "))
    }
}
