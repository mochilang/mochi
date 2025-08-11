var edges: MutableMap<String, MutableList<String>> = mutableMapOf<String, MutableList<String>>("a" to (mutableListOf("c", "b")), "b" to (mutableListOf("d", "e")), "c" to (mutableListOf<String>()), "d" to (mutableListOf<String>()), "e" to (mutableListOf<String>()))
var vertices: MutableList<String> = mutableListOf("a", "b", "c", "d", "e")
fun topological_sort(start: String, visited: MutableMap<String, Boolean>, sort: MutableList<String>): MutableList<String> {
    var sort: MutableList<String> = sort
    (visited)[start] = true
    var neighbors: MutableList<String> = (edges)[start] as MutableList<String>
    var i: Int = (0).toInt()
    while (i < neighbors.size) {
        var neighbor: String = neighbors[i]!!
        if (!(neighbor in visited)) {
            sort = topological_sort(neighbor, visited, sort)
        }
        i = i + 1
    }
    sort = run { val _tmp = sort.toMutableList(); _tmp.add(start); _tmp }
    if (visited.size != vertices.size) {
        var j: Int = (0).toInt()
        while (j < vertices.size) {
            var v: String = vertices[j]!!
            if (!(v in visited)) {
                sort = topological_sort(v, visited, sort)
            }
            j = j + 1
        }
    }
    return sort
}

fun user_main(): Unit {
    var result: MutableList<String> = topological_sort("a", mutableMapOf<String, Boolean>(), mutableListOf<String>())
    println(result.toString())
}

fun main() {
    user_main()
}
