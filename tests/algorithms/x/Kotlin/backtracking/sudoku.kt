var initial_grid: MutableList<MutableList<Int>> = mutableListOf(mutableListOf(3, 0, 6, 5, 0, 8, 4, 0, 0), mutableListOf(5, 2, 0, 0, 0, 0, 0, 0, 0), mutableListOf(0, 8, 7, 0, 0, 0, 0, 3, 1), mutableListOf(0, 0, 3, 0, 1, 0, 0, 8, 0), mutableListOf(9, 0, 0, 8, 6, 3, 0, 0, 5), mutableListOf(0, 5, 0, 0, 9, 0, 6, 0, 0), mutableListOf(1, 3, 0, 0, 0, 0, 2, 5, 0), mutableListOf(0, 0, 0, 0, 0, 0, 0, 7, 4), mutableListOf(0, 0, 5, 2, 0, 6, 3, 0, 0))
var no_solution: MutableList<MutableList<Int>> = mutableListOf(mutableListOf(5, 0, 6, 5, 0, 8, 4, 0, 3), mutableListOf(5, 2, 0, 0, 0, 0, 0, 0, 2), mutableListOf(1, 8, 7, 0, 0, 0, 0, 3, 1), mutableListOf(0, 0, 3, 0, 1, 0, 0, 8, 0), mutableListOf(9, 0, 0, 8, 6, 3, 0, 0, 5), mutableListOf(0, 5, 0, 0, 9, 0, 6, 0, 0), mutableListOf(1, 3, 0, 0, 0, 0, 2, 5, 0), mutableListOf(0, 0, 0, 0, 0, 0, 0, 7, 4), mutableListOf(0, 0, 5, 2, 0, 6, 3, 0, 0))
var examples: MutableList<MutableList<MutableList<Int>>> = mutableListOf(initial_grid, no_solution)
var idx: Int = 0
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

fun find_empty_location(grid: MutableList<MutableList<Int>>): MutableList<Int> {
    for (i in 0 until 9) {
        for (j in 0 until 9) {
            if ((((grid[i]!!) as MutableList<Int>))[j]!! == 0) {
                return mutableListOf(i, j)
            }
        }
    }
    return mutableListOf<Int>()
}

fun sudoku(grid: MutableList<MutableList<Int>>): Boolean {
    var loc: MutableList<Int> = find_empty_location(grid)
    if (loc.size == 0) {
        return true
    }
    var row: Int = loc[0]!!
    var column: Int = loc[1]!!
    for (digit in 1 until 10) {
        if (((is_safe(grid, row, column, digit)) as Boolean)) {
            (grid[row]!!)[column] = digit
            if (((sudoku(grid)) as Boolean)) {
                return true
            }
            (grid[row]!!)[column] = 0
        }
    }
    return false
}

fun print_solution(grid: MutableList<MutableList<Int>>): Unit {
    for (r in 0 until grid.size) {
        var line: String = ""
        for (c in 0 until (grid[r]!!).size) {
            line = line + ((((grid[r]!!) as MutableList<Int>))[c]!!).toString()
            if (c < ((grid[r]!!).size - 1)) {
                line = line + " "
            }
        }
        println(line)
    }
}

fun main() {
    while (idx < examples.size) {
        println("\nExample grid:\n====================")
        print_solution(examples[idx]!!)
        println("\nExample grid solution:")
        if (((sudoku(examples[idx]!!)) as Boolean)) {
            print_solution(examples[idx]!!)
        } else {
            println("Cannot find a solution.")
        }
        idx = idx + 1
    }
}
