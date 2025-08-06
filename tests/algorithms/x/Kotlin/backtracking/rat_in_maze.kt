import java.math.BigInteger

fun panic(msg: String): Nothing { throw RuntimeException(msg) }

var maze: MutableList<MutableList<Int>> = mutableListOf(mutableListOf(0, 1, 0, 1, 1), mutableListOf(0, 0, 0, 0, 0), mutableListOf(1, 0, 1, 0, 1), mutableListOf(0, 0, 1, 0, 0), mutableListOf(1, 0, 0, 1, 0))
var n: Int = maze.size - 1
fun run_maze(maze: MutableList<MutableList<Int>>, i: Int, j: Int, dr: Int, dc: Int, sol: MutableList<MutableList<Int>>): Boolean {
    var size: Int = maze.size
    if ((((i == dr) && (j == dc) as Boolean)) && ((((maze[i]!!) as MutableList<Int>))[j]!! == 0)) {
        (sol[i]!!)[j] = 0
        return true
    }
    var lower_flag: Boolean = (((i >= 0) && (j >= 0)) as Boolean)
    var upper_flag: Boolean = (((i < size) && (j < size)) as Boolean)
    if (lower_flag && upper_flag) {
        var block_flag: Boolean = ((((((sol[i]!!) as MutableList<Int>))[j]!! == 1) && ((((maze[i]!!) as MutableList<Int>))[j]!! == 0)) as Boolean)
        if ((block_flag as Boolean)) {
            (sol[i]!!)[j] = 0
            if (((((run_maze(maze, i + 1, j, dr, dc, sol) || run_maze(maze, i, j + 1, dr, dc, sol) as Boolean)) || run_maze(maze, i - 1, j, dr, dc, sol) as Boolean)) || run_maze(maze, i, j - 1, dr, dc, sol)) {
                return true
            }
            (sol[i]!!)[j] = 1
            return false
        }
    }
    return false
}

fun solve_maze(maze: MutableList<MutableList<Int>>, sr: Int, sc: Int, dr: Int, dc: Int): MutableList<MutableList<Int>> {
    var size: Int = maze.size
    if (!(((((((((((((((0 <= sr) && (sr < size) as Boolean)) && (0 <= sc) as Boolean)) && (sc < size) as Boolean)) && (0 <= dr) as Boolean)) && (dr < size) as Boolean)) && (0 <= dc) as Boolean)) && (dc < size)) as Boolean)) {
        panic("Invalid source or destination coordinates")
    }
    var sol: MutableList<MutableList<Int>> = mutableListOf<MutableList<Int>>()
    var i: Int = 0
    while (i < size) {
        var row: MutableList<Int> = mutableListOf<Int>()
        var j: Int = 0
        while (j < size) {
            row = run { val _tmp = row.toMutableList(); _tmp.add(1); _tmp }
            j = j + 1
        }
        sol = run { val _tmp = sol.toMutableList(); _tmp.add(row); _tmp }
        i = i + 1
    }
    var solved: Boolean = run_maze(maze, sr, sc, dr, dc, sol)
    if ((solved as Boolean)) {
        return sol
    } else {
        panic("No solution exists!")
    }
}

fun main() {
    println(solve_maze(maze, 0, 0, (n.toInt()), (n.toInt())).toString())
}
