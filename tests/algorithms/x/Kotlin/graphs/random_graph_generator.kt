import java.math.BigInteger

var seed: Int = (1).toInt()
fun rand(): Int {
    seed = (((Math.floorMod((((seed * 1103515245) + 12345).toLong()), 2147483648L)).toInt())).toInt()
    return seed
}

fun random(): Double {
    return (1.0 * rand()) / 2147483648.0
}

fun complete_graph(vertices_number: Int): MutableMap<Int, MutableList<Int>> {
    var graph: MutableMap<Int, MutableList<Int>> = mutableMapOf<Int, MutableList<Int>>()
    var i: Int = (0).toInt()
    while (i < vertices_number) {
        var neighbors: MutableList<Int> = mutableListOf<Int>()
        var j: Int = (0).toInt()
        while (j < vertices_number) {
            if (j != i) {
                neighbors = run { val _tmp = neighbors.toMutableList(); _tmp.add(j); _tmp }
            }
            j = j + 1
        }
        (graph)[i] = neighbors
        i = i + 1
    }
    return graph
}

fun random_graph(vertices_number: Int, probability: Double, directed: Boolean): MutableMap<Int, MutableList<Int>> {
    var graph: MutableMap<Int, MutableList<Int>> = mutableMapOf<Int, MutableList<Int>>()
    var i: Int = (0).toInt()
    while (i < vertices_number) {
        (graph)[i] = mutableListOf<Int>()
        i = i + 1
    }
    if (probability >= 1.0) {
        return complete_graph(vertices_number)
    }
    if (probability <= 0.0) {
        return graph
    }
    i = 0
    while (i < vertices_number) {
        var j: BigInteger = ((i + 1).toBigInteger())
        while (j.compareTo((vertices_number).toBigInteger()) < 0) {
            if (random() < probability) {
                (graph)[i] = run { val _tmp = ((graph)[i] as MutableList<Int>).toMutableList(); _tmp.add((j.toInt())); _tmp }
                if (!directed) {
                    (graph)[(j).toInt()] = run { val _tmp = ((graph)[(j).toInt()] as MutableList<Int>).toMutableList(); _tmp.add(i); _tmp }
                }
            }
            j = j.add((1).toBigInteger())
        }
        i = i + 1
    }
    return graph
}

fun user_main(): Unit {
    seed = (1).toInt()
    var g1: MutableMap<Int, MutableList<Int>> = random_graph(4, 0.5, false)
    println(g1)
    seed = (1).toInt()
    var g2: MutableMap<Int, MutableList<Int>> = random_graph(4, 0.5, true)
    println(g2)
}

fun main() {
    user_main()
}
