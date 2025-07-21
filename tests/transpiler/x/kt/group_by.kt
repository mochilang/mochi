data class GGroup(val key: Any, val items: MutableList<MutableMap<String, Any>>)
fun main() {
    val people = mutableListOf(mutableMapOf("name" to "Alice", "age" to 30, "city" to "Paris") as MutableMap<String, Any>, mutableMapOf("name" to "Bob", "age" to 15, "city" to "Hanoi") as MutableMap<String, Any>, mutableMapOf("name" to "Charlie", "age" to 65, "city" to "Paris") as MutableMap<String, Any>, mutableMapOf("name" to "Diana", "age" to 45, "city" to "Hanoi") as MutableMap<String, Any>, mutableMapOf("name" to "Eve", "age" to 70, "city" to "Paris") as MutableMap<String, Any>, mutableMapOf("name" to "Frank", "age" to 22, "city" to "Hanoi") as MutableMap<String, Any>)
    val stats = run {
    val _groups = mutableMapOf<Any, MutableList<MutableMap<String, Any>>>()
    for (person in people) {
        val _list = _groups.getOrPut(person["city"]!! as Any) { mutableListOf<MutableMap<String, Any>>() }
        _list.add(person)
    }
    val _res = mutableListOf<MutableMap<String, Any>>()
    for ((key, items) in _groups) {
        val g = GGroup(key, items)
        _res.add(mutableMapOf("city" to g.key, "count" to g.items.size, "avg_age" to (run {
    val _res = mutableListOf<Any>()
    for (p in g.items) {
        _res.add(p["age"]!!)
    }
    _res
}.map{(it as Number).toDouble()}).average()) as MutableMap<String, Any>)
    }
    _res
}
    println("--- People grouped by city ---")
    for (s in stats) {
        println(listOf(s["city"]!!, ": count =", s["count"]!!, ", avg_age =", s["avg_age"]!!).joinToString(" "))
    }
}
