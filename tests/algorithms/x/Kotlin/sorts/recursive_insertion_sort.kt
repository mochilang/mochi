fun <T> _listSet(lst: MutableList<T>, idx: Int, v: T) { while (lst.size <= idx) lst.add(v); lst[idx] = v }

fun panic(msg: String): Nothing { throw RuntimeException(msg) }

fun insert_next(collection: MutableList<Int>, index: Int): MutableList<Int> {
    var arr: MutableList<Int> = collection
    if ((index >= arr.size) || (arr[index - 1]!! <= arr[index]!!)) {
        return arr
    }
    var j: Int = (index - 1).toInt()
    var temp: Int = (arr[j]!!).toInt()
    _listSet(arr, j, arr[index]!!)
    _listSet(arr, index, temp)
    return insert_next(arr, index + 1)
}

fun rec_insertion_sort(collection: MutableList<Int>, n: Int): MutableList<Int> {
    var arr: MutableList<Int> = collection
    if ((arr.size <= 1) || (n <= 1)) {
        return arr
    }
    arr = insert_next(arr, n - 1)
    return rec_insertion_sort(arr, n - 1)
}

fun test_rec_insertion_sort(): Unit {
    var col1: MutableList<Int> = mutableListOf(1, 2, 1)
    col1 = rec_insertion_sort(col1, col1.size)
    if ((((col1[0]!! != 1) || (col1[1]!! != 1) as Boolean)) || (col1[2]!! != 2)) {
        panic("test1 failed")
    }
    var col2: MutableList<Int> = mutableListOf(2, 1, 0, 0 - 1, 0 - 2)
    col2 = rec_insertion_sort(col2, col2.size)
    if (col2[0]!! != (0 - 2)) {
        panic("test2 failed")
    }
    if (col2[1]!! != (0 - 1)) {
        panic("test2 failed")
    }
    if (col2[2]!! != 0) {
        panic("test2 failed")
    }
    if (col2[3]!! != 1) {
        panic("test2 failed")
    }
    if (col2[4]!! != 2) {
        panic("test2 failed")
    }
    var col3: MutableList<Int> = mutableListOf(1)
    col3 = rec_insertion_sort(col3, col3.size)
    if (col3[0]!! != 1) {
        panic("test3 failed")
    }
}

fun user_main(): Unit {
    test_rec_insertion_sort()
    var numbers: MutableList<Int> = mutableListOf(5, 3, 4, 1, 2)
    numbers = rec_insertion_sort(numbers, numbers.size)
    println(numbers.toString())
}

fun main() {
    user_main()
}
