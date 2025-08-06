import java.math.BigInteger

fun contains(xs: MutableList<Int>, x: Int): Boolean {
    var i: Int = 0
    while (i < xs.size) {
        if (xs[i]!! == x) {
            return true
        }
        i = i + 1
    }
    return false
}

fun get_point_key(len_board: Int, len_board_column: Int, row: Int, column: Int): Int {
    return ((len_board * len_board_column) * row) + column
}

fun search_from(board: MutableList<MutableList<String>>, word: String, row: Int, column: Int, word_index: Int, visited: MutableList<Int>): Boolean {
    if ((((board[row]!!) as MutableList<String>))[column]!! != word.substring(word_index, word_index + 1)) {
        return false
    }
    if (word_index == (word.length - 1)) {
        return true
    }
    var len_board: Int = board.size
    var len_board_column: Int = (board[0]!!).size
    var dir_i: MutableList<Int> = mutableListOf(0, 0, 0 - 1, 1)
    var dir_j: MutableList<Int> = mutableListOf(1, 0 - 1, 0, 0)
    var k: Int = 0
    while (k < 4) {
        var next_i: BigInteger = (row + dir_i[k]!!).toBigInteger()
        var next_j: BigInteger = (column + dir_j[k]!!).toBigInteger()
        if (!((((((((0).toBigInteger().compareTo((next_i)) <= 0) && (next_i.compareTo((len_board).toBigInteger()) < 0) as Boolean)) && ((0).toBigInteger().compareTo((next_j)) <= 0) as Boolean)) && (next_j.compareTo((len_board_column).toBigInteger()) < 0)) as Boolean)) {
            k = k + 1
            continue
        }
        var key: Int = get_point_key(len_board, len_board_column, (next_i.toInt()), (next_j.toInt()))
        if (visited.contains(key)) {
            k = k + 1
            continue
        }
        var new_visited = run { val _tmp = visited.toMutableList(); _tmp.add(key); _tmp }
        if (((search_from(board, word, (next_i.toInt()), (next_j.toInt()), word_index + 1, (new_visited as MutableList<Int>))) as Boolean)) {
            return true
        }
        k = k + 1
    }
    return false
}

fun word_exists(board: MutableList<MutableList<String>>, word: String): Boolean {
    var len_board: Int = board.size
    var len_board_column: Int = (board[0]!!).size
    var i: Int = 0
    while (i < len_board) {
        var j: Int = 0
        while (j < len_board_column) {
            var key: Int = get_point_key(len_board, len_board_column, i, j)
            var visited = run { val _tmp = (mutableListOf<Int>()).toMutableList(); _tmp.add(key); _tmp }
            if (((search_from(board, word, i, j, 0, (visited as MutableList<Int>))) as Boolean)) {
                return true
            }
            j = j + 1
        }
        i = i + 1
    }
    return false
}

fun user_main(): Unit {
    var board: MutableList<MutableList<String>> = mutableListOf(mutableListOf("A", "B", "C", "E"), mutableListOf("S", "F", "C", "S"), mutableListOf("A", "D", "E", "E"))
    println(word_exists(board, "ABCCED"))
    println(word_exists(board, "SEE"))
    println(word_exists(board, "ABCB"))
}

fun main() {
    user_main()
}
