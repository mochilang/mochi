fun main() {
    val data: MutableList<MutableMap<String, Int>> = mutableListOf(mutableMapOftype_arguments, mutableMapOftype_arguments, mutableMapOftype_arguments)
    val sorted: MutableList<MutableMap<String, Int>> = run {
        val _tmp = mutableListOftype_arguments
        for (x in data) {
            _tmp.add(Pair(mutableMapOftype_arguments, x))
        }
        val _res = _tmp.sortedBy {it.first}.map {it.second}.toMutableList()
        _res
    }
    println(sorted)
}
