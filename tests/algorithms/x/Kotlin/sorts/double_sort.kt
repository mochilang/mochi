import java.math.BigInteger

fun <T> _listSet(lst: MutableList<T>, idx: Int, v: T) { while (lst.size <= idx) lst.add(v); lst[idx] = v }

fun double_sort(collection: MutableList<Int>): MutableList<Int> {
    var no_of_elements: Int = (collection.size).toInt()
    var passes: Int = (((no_of_elements - 1) / 2) + 1).toInt()
    var i: Int = (0).toInt()
    while (i < passes) {
        var j: Int = (0).toInt()
        while (j < (no_of_elements - 1)) {
            if (collection[j + 1]!! < collection[j]!!) {
                var tmp: Int = (collection[j]!!).toInt()
                _listSet(collection, j, collection[j + 1]!!)
                _listSet(collection, j + 1, tmp)
            }
            if (collection[(no_of_elements - 1) - j]!! < collection[(no_of_elements - 2) - j]!!) {
                var tmp2: Int = (collection[(no_of_elements - 1) - j]!!).toInt()
                _listSet(collection, (no_of_elements - 1) - j, collection[(no_of_elements - 2) - j]!!)
                _listSet(collection, (no_of_elements - 2) - j, tmp2)
            }
            j = j + 1
        }
        i = i + 1
    }
    return collection
}

fun main() {
    println(double_sort(mutableListOf(0 - 1, 0 - 2, 0 - 3, 0 - 4, 0 - 5, 0 - 6, 0 - 7)).toString())
    println(double_sort(mutableListOf<Int>()).toString())
    println(double_sort(mutableListOf(0 - 1, 0 - 2, 0 - 3, 0 - 4, 0 - 5, 0 - 6)).toString())
    println((double_sort(mutableListOf(0 - 3, 10, 16, 0 - 42, 29)) == mutableListOf(0 - 42, 0 - 3, 10, 16, 29)).toString())
}
