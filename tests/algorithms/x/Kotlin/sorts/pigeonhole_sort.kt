import java.math.BigInteger

fun <T> _listSet(lst: MutableList<T>, idx: Int, v: T) { while (lst.size <= idx) lst.add(v); lst[idx] = v }

var example: MutableList<Int> = mutableListOf(8, 3, 2, 7, 4, 6, 8)
var result: MutableList<Int> = pigeonhole_sort(example)
var output: String = "Sorted order is:"
var j: Int = (0).toInt()
fun pigeonhole_sort(arr: MutableList<Int>): MutableList<Int> {
    if (arr.size == 0) {
        return arr
    }
    var min_val: Int = ((arr.min()!! as Int)).toInt()
    var max_val: Int = ((arr.max()!! as Int)).toInt()
    var size: Int = ((max_val - min_val) + 1).toInt()
    var holes: MutableList<Int> = mutableListOf<Int>()
    var i: Int = (0).toInt()
    while (i < size) {
        holes = run { val _tmp = holes.toMutableList(); _tmp.add(0); _tmp }
        i = i + 1
    }
    i = 0
    while (i < arr.size) {
        var x: Int = (arr[i]!!).toInt()
        var index: Int = (x - min_val).toInt()
        _listSet(holes, index, holes[index]!! + 1)
        i = i + 1
    }
    var sorted_index: Int = (0).toInt()
    var count: Int = (0).toInt()
    while (count < size) {
        while (holes[count]!! > 0) {
            _listSet(arr, sorted_index, count + min_val)
            _listSet(holes, count, holes[count]!! - 1)
            sorted_index = sorted_index + 1
        }
        count = count + 1
    }
    return arr
}

fun main() {
    while (j < result.size) {
        output = (output + " ") + (result[j]!!).toString()
        j = (j + 1).toInt()
    }
    println(output)
}
