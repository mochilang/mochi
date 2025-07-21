data class GGroup(val key: Any, val items: MutableList<MutableMap<String, String>>)
fun main() {
    val people = mutableListOf(mutableMapOf("name" to "Alice", "city" to "Paris"), mutableMapOf("name" to "Bob", "city" to "Hanoi"), mutableMapOf("name" to "Charlie", "city" to "Paris"), mutableMapOf("name" to "Diana", "city" to "Hanoi"), mutableMapOf("name" to "Eve", "city" to "Paris"), mutableMapOf("name" to "Frank", "city" to "Hanoi"), mutableMapOf("name" to "George", "city" to "Paris"))
    val big = run {
    val _groups = mutableMapOf<Any, MutableList<MutableMap<String, String>>>()
    for (p in people) {
        val _list = _groups.getOrPut(p["city"] as Any) { mutableListOf<MutableMap<String, String>>() }
        _list.add(p)
    }
    val _res = mutableListOf<MutableMap<String, Any>>()
    for ((key, items) in _groups) {
        val g = GGroup(key, items)
        if (g.items.size >= 4) {
            _res.add(mutableMapOf("city" to g.key, "num" to g.items.size))
        }
    }
    _res
}
    json(big)
}
