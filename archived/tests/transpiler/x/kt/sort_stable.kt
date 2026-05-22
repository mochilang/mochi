fun main() {
    val items: MutableList<MutableMap<String, Any>> = mutableListOf(mutableMapOf<String, Any>("n" to 1, "v" to "a"), mutableMapOf<String, Any>("n" to 1, "v" to "b"), mutableMapOf<String, Any>("n" to 2, "v" to "c"))
    val result: MutableList<Any> = run {
    val _tmp = mutableListOf<Pair<Any, Any>>()
    for (i in items) {
        _tmp.add(Pair((i["n"]!!), (i["v"]!!)))
    }
    val _res = _tmp.sortedBy { it.first }.map { it.second }.toMutableList()
    _res
}
    println(result)
}
