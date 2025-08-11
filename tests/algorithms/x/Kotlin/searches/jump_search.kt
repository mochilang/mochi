fun int_sqrt(n: Int): Int {
    var x: Int = (0).toInt()
    while (((x + 1) * (x + 1)) <= n) {
        x = x + 1
    }
    return x
}

fun jump_search(arr: MutableList<Int>, item: Int): Int {
    var arr_size: Int = (arr.size).toInt()
    var block_size: Int = (int_sqrt(arr_size)).toInt()
    var prev: Int = (0).toInt()
    var step: Int = (block_size).toInt()
    while ((step < arr_size) && (arr[step - 1]!! < item)) {
        prev = step
        step = step + block_size
        if (prev >= arr_size) {
            return 0 - 1
        }
    }
    while ((prev < arr_size) && (arr[prev]!! < item)) {
        prev = prev + 1
        if (prev == step) {
            return 0 - 1
        }
    }
    if ((prev < arr_size) && (arr[prev]!! == item)) {
        return prev
    }
    return 0 - 1
}

fun user_main(): Unit {
    println(jump_search(mutableListOf(0, 1, 2, 3, 4, 5), 3).toString())
    println(jump_search(mutableListOf(0 - 5, 0 - 2, 0 - 1), 0 - 1).toString())
    println(jump_search(mutableListOf(0, 5, 10, 20), 8).toString())
    println(jump_search(mutableListOf(0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233, 377, 610), 55).toString())
}

fun main() {
    user_main()
}
