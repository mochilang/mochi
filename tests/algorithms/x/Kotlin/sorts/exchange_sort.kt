import java.math.BigInteger

fun <T> _listSet(lst: MutableList<T>, idx: Int, v: T) { while (lst.size <= idx) lst.add(v); lst[idx] = v }

fun exchange_sort(numbers: MutableList<Int>): MutableList<Int> {
    var n: Int = (numbers.size).toInt()
    var i: Int = (0).toInt()
    while (i < n) {
        var j: BigInteger = ((i + 1).toBigInteger())
        while (j.compareTo((n).toBigInteger()) < 0) {
            if (numbers[(j).toInt()]!! < numbers[i]!!) {
                var temp: Int = (numbers[i]!!).toInt()
                _listSet(numbers, i, numbers[(j).toInt()]!!)
                _listSet(numbers, (j).toInt(), temp)
            }
            j = j.add((1).toBigInteger())
        }
        i = i + 1
    }
    return numbers
}

fun main() {
    println(exchange_sort(mutableListOf(5, 4, 3, 2, 1)).toString())
    println(exchange_sort(mutableListOf(0 - 1, 0 - 2, 0 - 3)).toString())
    println(exchange_sort(mutableListOf(1, 2, 3, 4, 5)).toString())
    println(exchange_sort(mutableListOf(0, 10, 0 - 2, 5, 3)).toString())
    println(exchange_sort(mutableListOf<Int>()).toString())
}
