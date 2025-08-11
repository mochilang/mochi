data class SearchProblem(var x: Double = 0.0, var y: Double = 0.0, var step: Double = 0.0, var f: ((Double, Double) -> Double)? = null)
fun score(sp: SearchProblem): Double {
    return (sp.f(sp.x, sp.y) as Double)
}

fun neighbors(sp: SearchProblem): MutableList<SearchProblem> {
    var s: Double = sp.step
    return mutableListOf(SearchProblem(x = sp.x - s, y = sp.y - s, step = s, f = sp.f), SearchProblem(x = sp.x - s, y = sp.y, step = s, f = sp.f), SearchProblem(x = sp.x - s, y = sp.y + s, step = s, f = sp.f), SearchProblem(x = sp.x, y = sp.y - s, step = s, f = sp.f), SearchProblem(x = sp.x, y = sp.y + s, step = s, f = sp.f), SearchProblem(x = sp.x + s, y = sp.y - s, step = s, f = sp.f), SearchProblem(x = sp.x + s, y = sp.y, step = s, f = sp.f), SearchProblem(x = sp.x + s, y = sp.y + s, step = s, f = sp.f))
}

fun equal_state(a: SearchProblem, b: SearchProblem): Boolean {
    return (((a.x == b.x) && (a.y == b.y)) as Boolean)
}

fun contains_state(lst: MutableList<SearchProblem>, sp: SearchProblem): Boolean {
    var i: Int = (0).toInt()
    while (i < lst.size) {
        if (((equal_state(lst[i]!!, sp)) as Boolean)) {
            return true
        }
        i = i + 1
    }
    return false
}

fun hill_climbing(sp: SearchProblem, find_max: Boolean, max_x: Double, min_x: Double, max_y: Double, min_y: Double, max_iter: Int): SearchProblem {
    var current: SearchProblem = sp
    var visited: MutableList<SearchProblem> = mutableListOf<SearchProblem>()
    var iterations: Int = (0).toInt()
    var solution_found: Boolean = false
    while ((solution_found == false) && (iterations < max_iter)) {
        visited = run { val _tmp = visited.toMutableList(); _tmp.add(current); _tmp }
        iterations = iterations + 1
        var current_score: Double = score(current)
        var neighs: MutableList<SearchProblem> = neighbors(current)
        var max_change: Double = 0.0 - 1000000000000000000.0
        var min_change: Double = 1000000000000000000.0
        var next: SearchProblem = current
        var improved: Boolean = false
        var i: Int = (0).toInt()
        while (i < neighs.size) {
            var n: SearchProblem = neighs[i]!!
            i = i + 1
            if (((contains_state(visited, n)) as Boolean)) {
                continue
            }
            if ((((((n.x > max_x) || (n.x < min_x) as Boolean)) || (n.y > max_y) as Boolean)) || (n.y < min_y)) {
                continue
            }
            var change: Double = score(n) - current_score
            if ((find_max as Boolean)) {
                if ((change > max_change) && (change > 0.0)) {
                    max_change = change
                    next = n
                    improved = true
                }
            } else {
                if ((change < min_change) && (change < 0.0)) {
                    min_change = change
                    next = n
                    improved = true
                }
            }
        }
        if ((improved as Boolean)) {
            current = next
        } else {
            solution_found = true
        }
    }
    return current
}

fun test_f1(x: Double, y: Double): Double {
    return (x * x) + (y * y)
}

fun user_main(): Unit {
    var prob1: SearchProblem = SearchProblem(x = 3.0, y = 4.0, step = 1.0, f = ::test_f1)
    var local_min1: SearchProblem = hill_climbing(prob1, false, 1000000000.0, 0.0 - 1000000000.0, 1000000000.0, 0.0 - 1000000000.0, 10000)
    println((((score(local_min1)).toInt())).toString())
    var prob2: SearchProblem = SearchProblem(x = 12.0, y = 47.0, step = 1.0, f = ::test_f1)
    var local_min2: SearchProblem = hill_climbing(prob2, false, 100.0, 5.0, 50.0, 0.0 - 5.0, 10000)
    println((((score(local_min2)).toInt())).toString())
    var prob3: SearchProblem = SearchProblem(x = 3.0, y = 4.0, step = 1.0, f = ::test_f1)
    var local_max: SearchProblem = hill_climbing(prob3, true, 1000000000.0, 0.0 - 1000000000.0, 1000000000.0, 0.0 - 1000000000.0, 1000)
    println((((score(local_max)).toInt())).toString())
}

fun main() {
    user_main()
}
