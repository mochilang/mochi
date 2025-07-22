fun main() {
    val data: MutableList<MutableMap<String, Int>> = mutableListOf(mutableMapOf<String, Int>("a" to 1, "b" to 2), mutableMapOf<String, Int>("a" to 1, "b" to 1), mutableMapOf<String, Int>("a" to 0, "b" to 5))
    val sorted: MutableList<MutableMap<String, Int>> = run {
    val _tmp = mutableListOf<Pair<MutableMap<String, Any>, MutableMap<String, Int>>>()
    for (x in data) {
        _tmp.add(Pair(mutableMapOf<String, Any>("a" to (x["a"]!!), "b" to (x["b"]!!)), x))
    }
    val _res = _tmp.sortedBy { it.first }.map { it.second }.toMutableList()
    _res
}
    println(sorted)
}
