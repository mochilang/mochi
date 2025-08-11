fun double_linear_search(array: MutableList<Int>, search_item: Int): Int {
    var start_ind: Int = (0).toInt()
    var end_ind: Int = (array.size - 1).toInt()
    while (start_ind <= end_ind) {
        if (array[start_ind]!! == search_item) {
            return start_ind
        }
        if (array[end_ind]!! == search_item) {
            return end_ind
        }
        start_ind = start_ind + 1
        end_ind = end_ind - 1
    }
    return 0 - 1
}

fun user_main(): Unit {
    var data: MutableList<Int> = build_range(100)
    println(double_linear_search(data, 40).toString())
}

fun build_range(n: Int): MutableList<Int> {
    var res: MutableList<Int> = mutableListOf<Int>()
    var i: Int = (0).toInt()
    while (i < n) {
        res = run { val _tmp = res.toMutableList(); _tmp.add(i); _tmp }
        i = i + 1
    }
    return res
}

fun main() {
    user_main()
}
