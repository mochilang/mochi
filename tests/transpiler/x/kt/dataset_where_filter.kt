fun main() {
    val people = mutableListOf(mutableMapOf("name" to "Alice", "age" to 30) as MutableMap<String, Any>, mutableMapOf("name" to "Bob", "age" to 15) as MutableMap<String, Any>, mutableMapOf("name" to "Charlie", "age" to 65) as MutableMap<String, Any>, mutableMapOf("name" to "Diana", "age" to 45) as MutableMap<String, Any>)
    val adults = run {
    val _res = mutableListOf<MutableMap<String, Any>>()
    for (person in people) {
        if (person["age"] >= 18) {
            _res.add(mutableMapOf("name" to person["name"], "age" to person["age"], "is_senior" to person["age"] >= 60) as MutableMap<String, Any>)
        }
    }
    _res
}
    println("--- Adults ---")
    for (person in adults) {
        println((((((person["name"] + " ") + "is") + " ") + person["age"]) + " ") + if (person["is_senior"] != null) " (senior)" else "")
    }
}
