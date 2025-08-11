fun <T> concat(a: MutableList<T>, b: MutableList<T>): MutableList<T> {
    val res = mutableListOf<T>()
    res.addAll(a)
    res.addAll(b)
    return res
}

fun quick_sort(items: MutableList<Int>): MutableList<Int> {
    if (items.size < 2) {
        return items
    }
    var pivot: Int = (items[0]!!).toInt()
    var lesser: MutableList<Int> = mutableListOf<Int>()
    var greater: MutableList<Int> = mutableListOf<Int>()
    var i: Int = (1).toInt()
    while (i < items.size) {
        var item: Int = (items[i]!!).toInt()
        if (item <= pivot) {
            lesser = run { val _tmp = lesser.toMutableList(); _tmp.add(item); _tmp }
        } else {
            greater = run { val _tmp = greater.toMutableList(); _tmp.add(item); _tmp }
        }
        i = i + 1
    }
    return ((concat(concat(quick_sort(lesser), mutableListOf(pivot)), quick_sort(greater))) as MutableList<Int>)
}

fun main() {
    println(listOf("sorted1:", quick_sort(mutableListOf(0, 5, 3, 2, 2))).joinToString(" "))
    println(listOf("sorted2:", quick_sort(mutableListOf<Int>())).joinToString(" "))
    println(listOf("sorted3:", quick_sort(mutableListOf(0 - 2, 5, 0, 0 - 45))).joinToString(" "))
}
