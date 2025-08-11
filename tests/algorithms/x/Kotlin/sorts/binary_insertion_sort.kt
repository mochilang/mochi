import java.math.BigInteger

fun <T> _listSet(lst: MutableList<T>, idx: Int, v: T) { while (lst.size <= idx) lst.add(v); lst[idx] = v }

fun binary_insertion_sort(arr: MutableList<Int>): MutableList<Int> {
    var i: Int = (1).toInt()
    while (i < arr.size) {
        var value: Int = (arr[i]!!).toInt()
        var low: Int = (0).toInt()
        var high: BigInteger = ((i - 1).toBigInteger())
        while ((low).toBigInteger().compareTo((high)) <= 0) {
            var mid = ((low).toBigInteger().add((high))).divide((2).toBigInteger())
            if (value < arr[(mid).toInt()]!!) {
                high = mid.subtract((1).toBigInteger())
            } else {
                low = ((mid.add((1).toBigInteger())).toInt())
            }
        }
        var j: Int = (i).toInt()
        while (j > low) {
            _listSet(arr, j, arr[j - 1]!!)
            j = j - 1
        }
        _listSet(arr, low, value)
        i = i + 1
    }
    return arr
}

fun user_main(): Unit {
    var example1: MutableList<Int> = mutableListOf(5, 2, 4, 6, 1, 3)
    println(binary_insertion_sort(example1).toString())
    var example2: MutableList<Int> = mutableListOf<Int>()
    println(binary_insertion_sort(example2).toString())
    var example3: MutableList<Int> = mutableListOf(4, 2, 4, 1, 3)
    println(binary_insertion_sort(example3).toString())
}

fun main() {
    user_main()
}
