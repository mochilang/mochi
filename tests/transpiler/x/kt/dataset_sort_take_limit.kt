fun main() {
    val products: MutableList<MutableMap<String, Any>> = mutableListOf(mutableMapOf<String, Any>("name" to "Laptop", "price" to 1500), mutableMapOf<String, Any>("name" to "Smartphone", "price" to 900), mutableMapOf<String, Any>("name" to "Tablet", "price" to 600), mutableMapOf<String, Any>("name" to "Monitor", "price" to 300), mutableMapOf<String, Any>("name" to "Keyboard", "price" to 100), mutableMapOf<String, Any>("name" to "Mouse", "price" to 50), mutableMapOf<String, Any>("name" to "Headphones", "price" to 200))
    val expensive: MutableList<MutableMap<String, Any>> = run {
    val _tmp = mutableListOf<Pair<Any, MutableMap<String, Any>>>()
    for (p in products) {
        _tmp.add(Pair(0 - (p["price"]!! as Number).toDouble(), p))
    }
    val _res = _tmp.sortedBy { it.first }.map { it.second }.toMutableList()
    _res
}.drop(1).take(3).toMutableList()
    println("--- Top products (excluding most expensive) ---")
    for (item in expensive) {
        println(listOf(item["name"]!!, "costs $", item["price"]!!).joinToString(" "))
    }
}
