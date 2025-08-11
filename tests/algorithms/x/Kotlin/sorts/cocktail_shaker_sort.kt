import java.math.BigInteger

fun <T> _listSet(lst: MutableList<T>, idx: Int, v: T) { while (lst.size <= idx) lst.add(v); lst[idx] = v }

fun cocktail_shaker_sort(arr: MutableList<Int>): MutableList<Int> {
    var start: Int = (0).toInt()
    var end: BigInteger = ((arr.size - 1).toBigInteger())
    while ((start).toBigInteger().compareTo((end)) < 0) {
        var swapped: Boolean = false
        var i: Int = (start).toInt()
        while ((i).toBigInteger().compareTo((end)) < 0) {
            if (arr[i]!! > arr[i + 1]!!) {
                var temp: Int = (arr[i]!!).toInt()
                _listSet(arr, i, arr[i + 1]!!)
                _listSet(arr, i + 1, temp)
                swapped = true
            }
            i = i + 1
        }
        if (!swapped) {
            break
        }
        end = end.subtract((1).toBigInteger())
        i = (end.toInt())
        while (i > start) {
            if (arr[i]!! < arr[i - 1]!!) {
                var temp2: Int = (arr[i]!!).toInt()
                _listSet(arr, i, arr[i - 1]!!)
                _listSet(arr, i - 1, temp2)
                swapped = true
            }
            i = i - 1
        }
        if (!swapped) {
            break
        }
        start = start + 1
    }
    return arr
}

fun main() {
    println(cocktail_shaker_sort(mutableListOf(4, 5, 2, 1, 2)).toString())
    println(cocktail_shaker_sort(mutableListOf(0 - 4, 5, 0, 1, 2, 11)).toString())
    println(cocktail_shaker_sort(mutableListOf(1, 2, 3, 4, 5)).toString())
}
