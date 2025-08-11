fun <T> _listSet(lst: MutableList<T>, idx: Int, v: T) { while (lst.size <= idx) lst.add(v); lst[idx] = v }

fun _len(v: Any?): Int = when (v) {
    is String -> v.length
    is Collection<*> -> v.size
    is Map<*, *> -> v.size
    else -> v.toString().length
}

data class Solution(var path: MutableList<String> = mutableListOf<String>(), var cost: Int = 0)
data class Swap(var a: String = "", var b: String = "")
var graph: MutableMap<String, MutableMap<String, Int>> = mutableMapOf<String, MutableMap<String, Int>>("a" to (mutableMapOf<String, Int>("b" to (20), "c" to (18), "d" to (22), "e" to (26))), "b" to (mutableMapOf<String, Int>("a" to (20), "c" to (10), "d" to (11), "e" to (12))), "c" to (mutableMapOf<String, Int>("a" to (18), "b" to (10), "d" to (23), "e" to (24))), "d" to (mutableMapOf<String, Int>("a" to (22), "b" to (11), "c" to (23), "e" to (40))), "e" to (mutableMapOf<String, Int>("a" to (26), "b" to (12), "c" to (24), "d" to (40))))
var first: Solution = generate_first_solution(graph, "a")
var best: Solution = tabu_search(first, graph, 4, 3)
fun path_cost(path: MutableList<String>, graph: MutableMap<String, MutableMap<String, Int>>): Int {
    var total: Int = (0).toInt()
    var i: Int = (0).toInt()
    while (i < (path.size - 1)) {
        var u: String = path[i]!!
        var v: String = path[i + 1]!!
        total = total + ((((graph)[u] as MutableMap<String, Int>) as MutableMap<String, Int>))[v] as Int
        i = i + 1
    }
    return total
}

fun generate_first_solution(graph: MutableMap<String, MutableMap<String, Int>>, start: String): Solution {
    var path: MutableList<String> = mutableListOf<String>()
    var visiting: String = start
    var total: Int = (0).toInt()
    while (path.size < graph.size) {
        path = run { val _tmp = path.toMutableList(); _tmp.add(visiting); _tmp }
        var best_node: String = ""
        var best_cost: Int = (1000000).toInt()
        for (n in (graph)[visiting] as MutableMap<String, Int>.keys) {
            if ((!(n in path) as Boolean) && (((((graph)[visiting] as MutableMap<String, Int>) as MutableMap<String, Int>))[n] as Int < best_cost)) {
                best_cost = ((((graph)[visiting] as MutableMap<String, Int>) as MutableMap<String, Int>))[n] as Int
                best_node = n
            }
        }
        if (best_node == "") {
            break
        }
        total = total + best_cost
        visiting = best_node
    }
    path = run { val _tmp = path.toMutableList(); _tmp.add(start); _tmp }
    total = total + ((((graph)[visiting] as MutableMap<String, Int>) as MutableMap<String, Int>))[start] as Int
    return Solution(path = path, cost = total)
}

fun copy_path(path: MutableList<String>): MutableList<String> {
    var res: MutableList<String> = mutableListOf<String>()
    var i: Int = (0).toInt()
    while (i < path.size) {
        res = run { val _tmp = res.toMutableList(); _tmp.add(path[i]!!); _tmp }
        i = i + 1
    }
    return res
}

fun find_neighborhood(sol: Solution, graph: MutableMap<String, MutableMap<String, Int>>): MutableList<Solution> {
    var neighbors: MutableList<Solution> = mutableListOf<Solution>()
    var i: Int = (1).toInt()
    while (i < ((sol.path).size - 1)) {
        var j: Int = (1).toInt()
        while (j < ((sol.path).size - 1)) {
            if (i != j) {
                var new_path: MutableList<String> = copy_path(sol.path)
                var tmp: String = new_path[i]!!
                _listSet(new_path, i, new_path[j]!!)
                _listSet(new_path, j, tmp)
                var cost: Int = (path_cost(new_path, graph)).toInt()
                neighbors = run { val _tmp = neighbors.toMutableList(); _tmp.add(Solution(path = new_path, cost = cost)); _tmp }
            }
            j = j + 1
        }
        i = i + 1
    }
    return neighbors
}

fun find_swap(a: MutableList<String>, b: MutableList<String>): Swap {
    var i: Int = (0).toInt()
    while (i < a.size) {
        if (a[i]!! != b[i]!!) {
            return Swap(a = a[i]!!, b = b[i]!!)
        }
        i = i + 1
    }
    return Swap(a = "", b = "")
}

fun tabu_search(first: Solution, graph: MutableMap<String, MutableMap<String, Int>>, iters: Int, size: Int): Solution {
    var solution: Solution = first
    var best: Solution = first
    var tabu: MutableList<Swap> = mutableListOf<Swap>()
    var count: Int = (0).toInt()
    while (count < iters) {
        var neighborhood: MutableList<Solution> = find_neighborhood((solution as Solution), graph)
        if (_len(neighborhood) == 0) {
            break
        }
        var best_neighbor = (neighborhood as MutableList<Any?>)[0]
        var best_move: Swap = find_swap(((solution as Solution)).path, ((best_neighbor as Solution)).path)
        var i: Int = (1).toInt()
        while (i < _len(neighborhood)) {
            var cand = (neighborhood as MutableList<Any?>)[i]
            var move: Swap = find_swap(((solution as Solution)).path, ((cand as Solution)).path)
            var forbidden: Boolean = false
            var t: Int = (0).toInt()
            while (t < tabu.size) {
                if ((((tabu[t]!!.a == ((move as Swap)).a) && (tabu[t]!!.b == ((move as Swap)).b) as Boolean)) || (((tabu[t]!!.a == ((move as Swap)).b) && (tabu[t]!!.b == ((move as Swap)).a) as Boolean))) {
                    forbidden = true
                }
                t = t + 1
            }
            if ((forbidden == false) && (((cand as Solution)).cost < ((best_neighbor as Solution)).cost)) {
                best_neighbor = cand
                best_move = move
            }
            i = i + 1
        }
        solution = best_neighbor
        tabu = run { val _tmp = tabu.toMutableList(); _tmp.add((best_move as Swap)); _tmp }
        if (tabu.size > size) {
            var new_tab: MutableList<Swap> = mutableListOf<Swap>()
            var j: Int = (1).toInt()
            while (j < tabu.size) {
                new_tab = run { val _tmp = new_tab.toMutableList(); _tmp.add(tabu[j]!!); _tmp }
                j = j + 1
            }
            tabu = new_tab
        }
        if (((solution as Solution)).cost < ((best as Solution)).cost) {
            best = solution
        }
        count = count + 1
    }
    return (best as Solution)
}

fun main() {
    println(((best as Solution)).path.toString())
    println(((best as Solution)).cost.toString())
}
