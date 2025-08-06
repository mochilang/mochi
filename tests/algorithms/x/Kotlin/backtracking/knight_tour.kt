import java.math.BigInteger

var board: MutableList<MutableList<Int>> = open_knight_tour(1)
fun get_valid_pos(position: MutableList<Int>, n: Int): MutableList<MutableList<Int>> {
    var y: Int = position[0]!!
    var x: Int = position[1]!!
    var positions: MutableList<MutableList<BigInteger>> = mutableListOf(mutableListOf(y + 1, x + 2), mutableListOf(y - 1, x + 2), mutableListOf(y + 1, x - 2), mutableListOf(y - 1, x - 2), mutableListOf(y + 2, x + 1), mutableListOf(y + 2, x - 1), mutableListOf(y - 2, x + 1), mutableListOf(y - 2, x - 1))
    var permissible: MutableList<MutableList<Int>> = mutableListOf<MutableList<Int>>()
    for (idx in 0 until positions.size) {
        var inner: MutableList<BigInteger> = positions[idx]!!
        var y_test: BigInteger = inner[0]!!
        var x_test: BigInteger = inner[1]!!
        if ((((((y_test.compareTo((0).toBigInteger()) >= 0) && (y_test.compareTo((n).toBigInteger()) < 0) as Boolean)) && (x_test.compareTo((0).toBigInteger()) >= 0) as Boolean)) && (x_test.compareTo((n).toBigInteger()) < 0)) {
            permissible = run { val _tmp = permissible.toMutableList(); _tmp.add((inner as MutableList<Int>)); _tmp }
        }
    }
    return permissible
}

fun is_complete(board: MutableList<MutableList<Int>>): Boolean {
    for (i in 0 until board.size) {
        var row: MutableList<Int> = board[i]!!
        for (j in 0 until row.size) {
            if (row[j]!! == 0) {
                return false
            }
        }
    }
    return true
}

fun open_knight_tour_helper(board: MutableList<MutableList<Int>>, pos: MutableList<Int>, curr: Int): Boolean {
    if (((is_complete(board)) as Boolean)) {
        return true
    }
    var moves: MutableList<MutableList<Int>> = get_valid_pos(pos, board.size)
    for (i in 0 until moves.size) {
        var position: MutableList<Int> = moves[i]!!
        var y: Int = position[0]!!
        var x: Int = position[1]!!
        if ((((board[y]!!) as MutableList<Int>))[x]!! == 0) {
            (board[y]!!)[x] = curr + 1
            if (((open_knight_tour_helper(board, position, curr + 1)) as Boolean)) {
                return true
            }
            (board[y]!!)[x] = 0
        }
    }
    return false
}

fun open_knight_tour(n: Int): MutableList<MutableList<Int>> {
    var board: MutableList<MutableList<Int>> = mutableListOf<MutableList<Int>>()
    for (i in 0 until n) {
        var row: MutableList<Int> = mutableListOf<Int>()
        for (j in 0 until n) {
            row = run { val _tmp = row.toMutableList(); _tmp.add(0); _tmp }
        }
        board = run { val _tmp = board.toMutableList(); _tmp.add(row); _tmp }
    }
    for (i in 0 until n) {
        for (j in 0 until n) {
            (board[i]!!)[j] = 1
            if (((open_knight_tour_helper(board, mutableListOf(i, j), 1)) as Boolean)) {
                return board
            }
            (board[i]!!)[j] = 0
        }
    }
    println("Open Knight Tour cannot be performed on a board of size " + n.toString())
    return board
}

fun main() {
    println((((board[0]!!) as MutableList<Int>))[0]!!)
}
