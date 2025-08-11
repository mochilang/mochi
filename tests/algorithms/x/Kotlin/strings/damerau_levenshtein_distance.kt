import java.math.BigInteger

fun <T> _listSet(lst: MutableList<T>, idx: Int, v: T) { while (lst.size <= idx) lst.add(v); lst[idx] = v }

fun damerau_levenshtein_distance(first_string: String, second_string: String): Int {
    var len1: Int = (first_string.length).toInt()
    var len2: Int = (second_string.length).toInt()
    var dp_matrix: MutableList<MutableList<Int>> = mutableListOf<MutableList<Int>>()
    for (_u1 in 0 until len1 + 1) {
        var row: MutableList<Int> = mutableListOf<Int>()
        for (_2 in 0 until len2 + 1) {
            row = run { val _tmp = row.toMutableList(); _tmp.add(0); _tmp }
        }
        dp_matrix = run { val _tmp = dp_matrix.toMutableList(); _tmp.add(row); _tmp }
    }
    for (i in 0 until len1 + 1) {
        var row: MutableList<Int> = dp_matrix[i]!!
        _listSet(row, 0, i)
        _listSet(dp_matrix, i, row)
    }
    var first_row: MutableList<Int> = dp_matrix[0]!!
    for (j in 0 until len2 + 1) {
        _listSet(first_row, j, j)
    }
    _listSet(dp_matrix, 0, first_row)
    for (i in 1 until len1 + 1) {
        var row: MutableList<Int> = dp_matrix[i]!!
        var first_char: String = first_string.substring(i - 1, i)
        for (j in 1 until len2 + 1) {
            var second_char: String = second_string.substring(j - 1, j)
            var cost: Int = (if (first_char == second_char) 0 else 1).toInt()
            var value: BigInteger = (((((dp_matrix[i - 1]!!) as MutableList<Int>))[j]!! + 1).toBigInteger())
            var insertion: Int = (row[j - 1]!! + 1).toInt()
            if ((insertion).toBigInteger().compareTo((value)) < 0) {
                value = (insertion.toBigInteger())
            }
            var substitution: Int = ((((dp_matrix[i - 1]!!) as MutableList<Int>))[j - 1]!! + cost).toInt()
            if ((substitution).toBigInteger().compareTo((value)) < 0) {
                value = (substitution.toBigInteger())
            }
            _listSet(row, j, (value.toInt()))
            if ((((((i > 1) && (j > 1) as Boolean)) && (first_string.substring(i - 1, i) == second_string.substring(j - 2, j - 1)) as Boolean)) && (first_string.substring(i - 2, i - 1) == second_string.substring(j - 1, j))) {
                var transposition: Int = ((((dp_matrix[i - 2]!!) as MutableList<Int>))[j - 2]!! + cost).toInt()
                if (transposition < row[j]!!) {
                    _listSet(row, j, transposition)
                }
            }
        }
        _listSet(dp_matrix, i, row)
    }
    return (((dp_matrix[len1]!!) as MutableList<Int>))[len2]!!
}

fun main() {
    println(damerau_levenshtein_distance("cat", "cut").toString())
    println(damerau_levenshtein_distance("kitten", "sitting").toString())
    println(damerau_levenshtein_distance("hello", "world").toString())
    println(damerau_levenshtein_distance("book", "back").toString())
    println(damerau_levenshtein_distance("container", "containment").toString())
}
