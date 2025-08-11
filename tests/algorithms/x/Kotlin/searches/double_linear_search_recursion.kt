fun search(list_data: MutableList<Int>, key: Int, left: Int, right: Int): Int {
    var r: Int = (right).toInt()
    if (r == 0) {
        r = list_data.size - 1
    }
    if (left > r) {
        return 0 - 1
    } else {
        if (list_data[left]!! == key) {
            return left
        } else {
            if (list_data[r]!! == key) {
                return r
            } else {
                return search(list_data, key, left + 1, r - 1)
            }
        }
    }
}

fun user_main(): Unit {
    println(search(mutableListOf(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10), 5, 0, 0))
    println(search(mutableListOf(1, 2, 4, 5, 3), 4, 0, 0))
    println(search(mutableListOf(1, 2, 4, 5, 3), 6, 0, 0))
    println(search(mutableListOf(5), 5, 0, 0))
    println(search(mutableListOf<Int>(), 1, 0, 0))
}

fun main() {
    user_main()
}
