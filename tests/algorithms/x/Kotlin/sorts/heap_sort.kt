import java.math.BigInteger

fun <T> _listSet(lst: MutableList<T>, idx: Int, v: T) { while (lst.size <= idx) lst.add(v); lst[idx] = v }

fun panic(msg: String): Nothing { throw RuntimeException(msg) }

var data: MutableList<Int> = mutableListOf(3, 7, 9, 28, 123, 0 - 5, 8, 0 - 30, 0 - 200, 0, 4)
var result: MutableList<Int> = heap_sort(data)
fun heapify(arr: MutableList<Int>, index: Int, heap_size: Int): Unit {
    var largest: Int = (index).toInt()
    var left_index: Int = ((2 * index) + 1).toInt()
    var right_index: Int = ((2 * index) + 2).toInt()
    if ((left_index < heap_size) && (arr[left_index]!! > arr[largest]!!)) {
        largest = left_index
    }
    if ((right_index < heap_size) && (arr[right_index]!! > arr[largest]!!)) {
        largest = right_index
    }
    if (largest != index) {
        var temp: Int = (arr[largest]!!).toInt()
        _listSet(arr, largest, arr[index]!!)
        _listSet(arr, index, temp)
        heapify(arr, largest, heap_size)
    }
}

fun heap_sort(arr: MutableList<Int>): MutableList<Int> {
    var n: Int = (arr.size).toInt()
    var i: BigInteger = (((n / 2) - 1).toBigInteger())
    while (i.compareTo((0).toBigInteger()) >= 0) {
        heapify(arr, (i.toInt()), n)
        i = i.subtract((1).toBigInteger())
    }
    i = ((n - 1).toBigInteger())
    while (i.compareTo((0).toBigInteger()) > 0) {
        var temp: Int = (arr[0]!!).toInt()
        _listSet(arr, 0, arr[(i).toInt()]!!)
        _listSet(arr, (i).toInt(), temp)
        heapify(arr, 0, (i.toInt()))
        i = i.subtract((1).toBigInteger())
    }
    return arr
}

fun main() {
    println(result)
    if (result.toString() != mutableListOf(0 - 200, 0 - 30, 0 - 5, 0, 3, 4, 7, 8, 9, 28, 123).toString()) {
        panic("Assertion error")
    }
}
