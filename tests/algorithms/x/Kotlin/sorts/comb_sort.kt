fun <T> _listSet(lst: MutableList<T>, idx: Int, v: T) { while (lst.size <= idx) lst.add(v); lst[idx] = v }

fun comb_sort(data: MutableList<Int>): MutableList<Int> {
    var shrink_factor: Double = 1.3
    var gap: Int = (data.size).toInt()
    var completed: Boolean = false
    while (!completed) {
        gap = ((gap / shrink_factor).toInt())
        if (gap <= 1) {
            gap = 1
            completed = true
        }
        var index: Int = (0).toInt()
        while ((index + gap) < data.size) {
            if (data[index]!! > data[index + gap]!!) {
                var tmp: Int = (data[index]!!).toInt()
                _listSet(data, index, data[index + gap]!!)
                _listSet(data, index + gap, tmp)
                completed = false
            }
            index = index + 1
        }
    }
    return data
}

fun user_main(): Unit {
    println(comb_sort(mutableListOf(0, 5, 3, 2, 2)))
    println(comb_sort(mutableListOf<Int>()))
    println(comb_sort(mutableListOf(99, 45, 0 - 7, 8, 2, 0, 0 - 15, 3)))
}

fun main() {
    user_main()
}
