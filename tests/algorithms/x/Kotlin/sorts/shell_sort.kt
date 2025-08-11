fun <T> _listSet(lst: MutableList<T>, idx: Int, v: T) { while (lst.size <= idx) lst.add(v); lst[idx] = v }

fun shell_sort(collection: MutableList<Int>): MutableList<Int> {
    var arr: MutableList<Int> = collection
    var gaps: MutableList<Int> = mutableListOf(701, 301, 132, 57, 23, 10, 4, 1)
    var g: Int = (0).toInt()
    while (g < gaps.size) {
        var gap: Int = (gaps[g]!!).toInt()
        var i: Int = (gap).toInt()
        while (i < arr.size) {
            var insert_value: Int = (arr[i]!!).toInt()
            var j: Int = (i).toInt()
            while ((j >= gap) && (arr[j - gap]!! > insert_value)) {
                _listSet(arr, j, arr[j - gap]!!)
                j = j - gap
            }
            if (j != i) {
                _listSet(arr, j, insert_value)
            }
            i = i + 1
        }
        g = g + 1
    }
    return arr
}

fun main() {
    println(shell_sort(mutableListOf(0, 5, 3, 2, 2)).toString())
    println(shell_sort(mutableListOf<Int>()).toString())
    println(shell_sort(mutableListOf(0 - 2, 0 - 5, 0 - 45)).toString())
}
