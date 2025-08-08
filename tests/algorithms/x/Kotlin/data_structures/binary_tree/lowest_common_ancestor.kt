fun pow2(n: Int): Int {
var v = 1
var i = 0
while (i < n) {
v *= 2
i++
}
return v
}

fun <T> _listSet(lst: MutableList<T>, idx: Int, v: T) { while (lst.size <= idx) lst.add(v); lst[idx] = v }

fun create_sparse(max_node: Int, parent: MutableList<MutableList<Int>>): MutableList<MutableList<Int>> {
    var j: Int = (1).toInt()
    while (pow2(j) < max_node) {
        var i: Int = (1).toInt()
        while (i <= max_node) {
            _listSet(parent[j]!!, i, (((parent[j - 1]!!) as MutableList<Int>))[(((parent[j - 1]!!) as MutableList<Int>))[i]!!]!!)
            i = i + 1
        }
        j = j + 1
    }
    return parent
}

fun lowest_common_ancestor(u: Int, v: Int, level: MutableList<Int>, parent: MutableList<MutableList<Int>>): Int {
    var v: Int = (v).toInt()
    var u: Int = (u).toInt()
    if (level[u]!! < level[v]!!) {
        var temp: Int = (u).toInt()
        u = v
        v = temp
    }
    var i: Int = (18).toInt()
    while (i >= 0) {
        if ((level[u]!! - pow2(i)) >= level[v]!!) {
            u = (((parent[i]!!) as MutableList<Int>))[u]!!
        }
        i = i - 1
    }
    if (u == v) {
        return u
    }
    i = 18
    while (i >= 0) {
        var pu: Int = ((((parent[i]!!) as MutableList<Int>))[u]!!).toInt()
        var pv: Int = ((((parent[i]!!) as MutableList<Int>))[v]!!).toInt()
        if ((pu != 0) && (pu != pv)) {
            u = pu
            v = pv
        }
        i = i - 1
    }
    return (((parent[0]!!) as MutableList<Int>))[u]!!
}

fun breadth_first_search(level: MutableList<Int>, parent: MutableList<MutableList<Int>>, max_node: Int, graph: MutableMap<Int, MutableList<Int>>, root: Int): Unit {
    _listSet(level, root, 0)
    var q: MutableList<Int> = mutableListOf<Int>()
    q = run { val _tmp = q.toMutableList(); _tmp.add(root); _tmp }
    var head: Int = (0).toInt()
    while (head < q.size) {
        var u: Int = (q[head]!!).toInt()
        head = head + 1
        var adj: MutableList<Int> = (graph)[u] as MutableList<Int>
        var j: Int = (0).toInt()
        while (j < adj.size) {
            var v: Int = (adj[j]!!).toInt()
            if (level[v]!! == (0 - 1)) {
                _listSet(level, v, level[u]!! + 1)
                _listSet(parent[0]!!, v, u)
                q = run { val _tmp = q.toMutableList(); _tmp.add(v); _tmp }
            }
            j = j + 1
        }
    }
}

fun user_main(): Unit {
    var max_node: Int = (13).toInt()
    var parent: MutableList<MutableList<Int>> = mutableListOf<MutableList<Int>>()
    var i: Int = (0).toInt()
    while (i < 20) {
        var row: MutableList<Int> = mutableListOf<Int>()
        var j: Int = (0).toInt()
        while (j < (max_node + 10)) {
            row = run { val _tmp = row.toMutableList(); _tmp.add(0); _tmp }
            j = j + 1
        }
        parent = run { val _tmp = parent.toMutableList(); _tmp.add(row); _tmp }
        i = i + 1
    }
    var level: MutableList<Int> = mutableListOf<Int>()
    i = 0
    while (i < (max_node + 10)) {
        level = run { val _tmp = level.toMutableList(); _tmp.add(0 - 1); _tmp }
        i = i + 1
    }
    var graph: MutableMap<Int, MutableList<Int>> = mutableMapOf<Int, MutableList<Int>>()
    (graph)[1] = mutableListOf(2, 3, 4)
    (graph)[2] = mutableListOf(5)
    (graph)[3] = mutableListOf(6, 7)
    (graph)[4] = mutableListOf(8)
    (graph)[5] = mutableListOf(9, 10)
    (graph)[6] = mutableListOf(11)
    (graph)[7] = mutableListOf<Int>()
    (graph)[8] = mutableListOf(12, 13)
    (graph)[9] = mutableListOf<Int>()
    (graph)[10] = mutableListOf<Int>()
    (graph)[11] = mutableListOf<Int>()
    (graph)[12] = mutableListOf<Int>()
    (graph)[13] = mutableListOf<Int>()
    breadth_first_search(level, parent, max_node, graph, 1)
    parent = create_sparse(max_node, parent)
    println("LCA of node 1 and 3 is: " + lowest_common_ancestor(1, 3, level, parent).toString())
    println("LCA of node 5 and 6 is: " + lowest_common_ancestor(5, 6, level, parent).toString())
    println("LCA of node 7 and 11 is: " + lowest_common_ancestor(7, 11, level, parent).toString())
    println("LCA of node 6 and 7 is: " + lowest_common_ancestor(6, 7, level, parent).toString())
    println("LCA of node 4 and 12 is: " + lowest_common_ancestor(4, 12, level, parent).toString())
    println("LCA of node 8 and 8 is: " + lowest_common_ancestor(8, 8, level, parent).toString())
}

fun main() {
    user_main()
}
