fun <T> _listSet(lst: MutableList<T>, idx: Int, v: T) { while (lst.size <= idx) lst.add(v); lst[idx] = v }

fun gnome_sort(lst: MutableList<Int>): MutableList<Int> {
    if (lst.size <= 1) {
        return lst
    }
    var i: Int = (1).toInt()
    while (i < lst.size) {
        if (lst[i - 1]!! <= lst[i]!!) {
            i = i + 1
        } else {
            var tmp: Int = (lst[i - 1]!!).toInt()
            _listSet(lst, i - 1, lst[i]!!)
            _listSet(lst, i, tmp)
            i = i - 1
            if (i == 0) {
                i = 1
            }
        }
    }
    return lst
}

fun main() {
    println(gnome_sort(mutableListOf(0, 5, 3, 2, 2)))
    println(gnome_sort(mutableListOf<Int>()))
    println(gnome_sort(mutableListOf(0 - 2, 0 - 5, 0 - 45)))
}
