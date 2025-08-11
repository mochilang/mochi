fun <T> _listSet(lst: MutableList<T>, idx: Int, v: T) { while (lst.size <= idx) lst.add(v); lst[idx] = v }

fun shell_sort(collection: MutableList<Int>): MutableList<Int> {
    var gap: Int = (collection.size).toInt()
    var ten: Int = (10).toInt()
    var thirteen: Int = (13).toInt()
    while (gap > 1) {
        gap = (gap * ten) / thirteen
        var i: Int = (gap).toInt()
        while (i < collection.size) {
            var temp: Int = (collection[i]!!).toInt()
            var j: Int = (i).toInt()
            while ((j >= gap) && (collection[j - gap]!! > temp)) {
                _listSet(collection, j, collection[j - gap]!!)
                j = j - gap
            }
            _listSet(collection, j, temp)
            i = i + 1
        }
    }
    return collection
}

fun user_main(): Unit {
    println(shell_sort(mutableListOf(3, 2, 1)).toString())
    println(shell_sort(mutableListOf<Int>()).toString())
    println(shell_sort(mutableListOf(1)).toString())
}

fun main() {
    user_main()
}
