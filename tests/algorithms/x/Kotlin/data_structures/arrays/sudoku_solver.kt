fun <T> _listSet(lst: MutableList<T>, idx: Int, v: T) { while (lst.size <= idx) lst.add(v); lst[idx] = v }

var puzzle: String = "003020600900305001001806400008102900700000008006708200002609500800203009005010300"
var grid: MutableList<MutableList<Int>> = string_to_grid(puzzle)
fun string_to_grid(s: String): MutableList<MutableList<Int>> {
    var grid: MutableList<MutableList<Int>> = mutableListOf<MutableList<Int>>()
    var i: Int = (0).toInt()
    while (i < 9) {
        var row: MutableList<Int> = mutableListOf<Int>()
        var j: Int = (0).toInt()
        while (j < 9) {
            var ch: String = s.substring((i * 9) + j, ((i * 9) + j) + 1)
            var _val: Int = (0).toInt()
            if ((ch != "0") && (ch != ".")) {
                _val = (ch.toBigInteger().toInt())
            }
            row = run { val _tmp = row.toMutableList(); _tmp.add(_val); _tmp }
            j = j + 1
        }
        grid = run { val _tmp = grid.toMutableList(); _tmp.add(row); _tmp }
        i = i + 1
    }
    return grid
}

fun print_grid(grid: MutableList<MutableList<Int>>): Unit {
    for (r in 0 until 9) {
        var line: String = ""
        for (c in 0 until 9) {
            line = line + ((((grid[r]!!) as MutableList<Int>))[c]!!).toString()
            if (c < 8) {
                line = line + " "
            }
        }
        println(line)
    }
}

fun is_safe(grid: MutableList<MutableList<Int>>, row: Int, column: Int, n: Int): Boolean {
    for (i in 0 until 9) {
        if (((((grid[row]!!) as MutableList<Int>))[i]!! == n) || ((((grid[i]!!) as MutableList<Int>))[column]!! == n)) {
            return false
        }
    }
    for (i in 0 until 3) {
        for (j in 0 until 3) {
            if ((((grid[(row - (Math.floorMod(row, 3))) + i]!!) as MutableList<Int>))[(column - (Math.floorMod(column, 3))) + j]!! == n) {
                return false
            }
        }
    }
    return true
}

fun find_empty(grid: MutableList<MutableList<Int>>): MutableList<Int> {
    for (i in 0 until 9) {
        for (j in 0 until 9) {
            if ((((grid[i]!!) as MutableList<Int>))[j]!! == 0) {
                return mutableListOf(i, j)
            }
        }
    }
    return mutableListOf<Int>()
}

fun solve(grid: MutableList<MutableList<Int>>): Boolean {
    var loc: MutableList<Int> = find_empty(grid)
    if (loc.size == 0) {
        return true
    }
    var row: Int = (loc[0]!!).toInt()
    var column: Int = (loc[1]!!).toInt()
    for (digit in 1 until 10) {
        if (((is_safe(grid, row, column, digit)) as Boolean)) {
            _listSet(grid[row]!!, column, digit)
            if (((solve(grid)) as Boolean)) {
                return true
            }
            _listSet(grid[row]!!, column, 0)
        }
    }
    return false
}

fun main() {
    println("Original grid:")
    print_grid(grid)
    if (((solve(grid)) as Boolean)) {
        println("\nSolved grid:")
        print_grid(grid)
    } else {
        println("\nNo solution found")
    }
}
