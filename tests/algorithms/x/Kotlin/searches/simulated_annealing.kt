var _nowSeed = 0L
var _nowSeeded = false
fun _now(): Long {
    if (!_nowSeeded) {
        System.getenv("MOCHI_NOW_SEED")?.toLongOrNull()?.let {
            _nowSeed = it
            _nowSeeded = true
        }
    }
    return if (_nowSeeded) {
        _nowSeed = (_nowSeed * 1664525 + 1013904223) % 2147483647
        kotlin.math.abs(_nowSeed)
    } else {
        kotlin.math.abs(System.nanoTime())
    }
}

data class SearchProblem(var x: Double = 0.0, var y: Double = 0.0, var step: Double = 0.0)
var seed: Int = (1).toInt()
fun score(p: SearchProblem, f: (Double, Double) -> Double): Double {
    return ((f(p.x, p.y)).toDouble())
}

fun get_neighbors(p: SearchProblem): MutableList<SearchProblem> {
    var s: Double = p.step
    var ns: MutableList<SearchProblem> = mutableListOf<SearchProblem>()
    ns = run { val _tmp = ns.toMutableList(); _tmp.add(SearchProblem(x = p.x - s, y = p.y - s, step = s)); _tmp }
    ns = run { val _tmp = ns.toMutableList(); _tmp.add(SearchProblem(x = p.x - s, y = p.y, step = s)); _tmp }
    ns = run { val _tmp = ns.toMutableList(); _tmp.add(SearchProblem(x = p.x - s, y = p.y + s, step = s)); _tmp }
    ns = run { val _tmp = ns.toMutableList(); _tmp.add(SearchProblem(x = p.x, y = p.y - s, step = s)); _tmp }
    ns = run { val _tmp = ns.toMutableList(); _tmp.add(SearchProblem(x = p.x, y = p.y + s, step = s)); _tmp }
    ns = run { val _tmp = ns.toMutableList(); _tmp.add(SearchProblem(x = p.x + s, y = p.y - s, step = s)); _tmp }
    ns = run { val _tmp = ns.toMutableList(); _tmp.add(SearchProblem(x = p.x + s, y = p.y, step = s)); _tmp }
    ns = run { val _tmp = ns.toMutableList(); _tmp.add(SearchProblem(x = p.x + s, y = p.y + s, step = s)); _tmp }
    return ns
}

fun remove_at(lst: MutableList<SearchProblem>, idx: Int): MutableList<SearchProblem> {
    var res: MutableList<SearchProblem> = mutableListOf<SearchProblem>()
    var i: Int = (0).toInt()
    while (i < lst.size) {
        if (i != idx) {
            res = run { val _tmp = res.toMutableList(); _tmp.add(lst[i]!!); _tmp }
        }
        i = i + 1
    }
    return res
}

fun rand(): Int {
    var _t: Long = (_now().toLong())
    seed = (((Math.floorMod((((seed * 1103515245) + 12345).toLong()), 2147483648L)).toInt())).toInt()
    return seed
}

fun random_float(): Double {
    return (((rand()).toDouble())) / 2147483648.0
}

fun randint(low: Int, high: Int): Int {
    return (Math.floorMod(rand(), ((high - low) + 1))) + low
}

fun expApprox(x: Double): Double {
    var y: Double = x
    var is_neg: Boolean = false
    if (x < 0.0) {
        is_neg = true
        y = 0.0 - x
    }
    var term: Double = 1.0
    var sum: Double = 1.0
    var n: Int = (1).toInt()
    while (n < 30) {
        term = (term * y) / ((n.toDouble()))
        sum = sum + term
        n = n + 1
    }
    if ((is_neg as Boolean)) {
        return 1.0 / sum
    }
    return sum
}

fun simulated_annealing(search_prob: SearchProblem, f: (Double, Double) -> Double, find_max: Boolean, max_x: Double, min_x: Double, max_y: Double, min_y: Double, start_temp: Double, rate_of_decrease: Double, threshold_temp: Double): SearchProblem {
    var search_end: Boolean = false
    var current_state: SearchProblem = search_prob
    var current_temp: Double = start_temp
    var best_state: SearchProblem = current_state
    while (!search_end) {
        var current_score: Double = score(current_state, f)
        if (score(best_state, f) < current_score) {
            best_state = current_state
        }
        var next_state: SearchProblem = current_state
        var found_next: Boolean = false
        var neighbors: MutableList<SearchProblem> = get_neighbors(current_state)
        while ((!found_next as Boolean) && (neighbors.size > 0)) {
            var idx: Int = (randint(0, neighbors.size - 1)).toInt()
            var picked_neighbor: SearchProblem = neighbors[idx]!!
            neighbors = remove_at(neighbors, idx)
            if ((((((picked_neighbor.x > max_x) || (picked_neighbor.x < min_x) as Boolean)) || (picked_neighbor.y > max_y) as Boolean)) || (picked_neighbor.y < min_y)) {
                continue
            }
            var change: Double = score(picked_neighbor, f) - current_score
            if (!find_max) {
                change = 0.0 - change
            }
            if (change > 0.0) {
                next_state = picked_neighbor
                found_next = true
            } else {
                var probability: Double = expApprox(change / current_temp)
                if (random_float() < probability) {
                    next_state = picked_neighbor
                    found_next = true
                }
            }
        }
        current_temp = current_temp - (current_temp * rate_of_decrease)
        if ((current_temp < threshold_temp) || (!found_next as Boolean)) {
            search_end = true
        } else {
            current_state = next_state
        }
    }
    return best_state
}

fun test_f1(x: Double, y: Double): Double {
    return (x * x) + (y * y)
}

fun test_f2(x: Double, y: Double): Double {
    return ((3.0 * x) * x) - (6.0 * y)
}

fun user_main(): Unit {
    var prob1: SearchProblem = SearchProblem(x = 12.0, y = 47.0, step = 1.0)
    var min_state: SearchProblem = simulated_annealing(prob1, ::test_f1, false, 100.0, 5.0, 50.0, 0.0 - 5.0, 100.0, 0.01, 1.0)
    println(listOf("min1", test_f1(min_state.x, min_state.y)).joinToString(" "))
    var prob2: SearchProblem = SearchProblem(x = 12.0, y = 47.0, step = 1.0)
    var max_state: SearchProblem = simulated_annealing(prob2, ::test_f1, true, 100.0, 5.0, 50.0, 0.0 - 5.0, 100.0, 0.01, 1.0)
    println(listOf("max1", test_f1(max_state.x, max_state.y)).joinToString(" "))
    var prob3: SearchProblem = SearchProblem(x = 3.0, y = 4.0, step = 1.0)
    var min_state2: SearchProblem = simulated_annealing(prob3, ::test_f2, false, 1000.0, 0.0 - 1000.0, 1000.0, 0.0 - 1000.0, 100.0, 0.01, 1.0)
    println(listOf("min2", test_f2(min_state2.x, min_state2.y)).joinToString(" "))
    var prob4: SearchProblem = SearchProblem(x = 3.0, y = 4.0, step = 1.0)
    var max_state2: SearchProblem = simulated_annealing(prob4, ::test_f2, true, 1000.0, 0.0 - 1000.0, 1000.0, 0.0 - 1000.0, 100.0, 0.01, 1.0)
    println(listOf("max2", test_f2(max_state2.x, max_state2.y)).joinToString(" "))
}

fun main() {
    user_main()
}
