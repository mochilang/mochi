import java.math.BigInteger

fun <T> _listSet(lst: MutableList<T>, idx: Int, v: T) { while (lst.size <= idx) lst.add(v); lst[idx] = v }

fun cycle_sort(arr: MutableList<Int>): MutableList<Int> {
    var n: Int = (arr.size).toInt()
    var cycle_start: Int = (0).toInt()
    while (cycle_start < (n - 1)) {
        var item: Int = (arr[cycle_start]!!).toInt()
        var pos: Int = (cycle_start).toInt()
        var i: BigInteger = ((cycle_start + 1).toBigInteger())
        while (i.compareTo((n).toBigInteger()) < 0) {
            if (arr[(i).toInt()]!! < item) {
                pos = pos + 1
            }
            i = i.add((1).toBigInteger())
        }
        if (pos == cycle_start) {
            cycle_start = cycle_start + 1
            continue
        }
        while (item == arr[pos]!!) {
            pos = pos + 1
        }
        var temp: Int = (arr[pos]!!).toInt()
        _listSet(arr, pos, item)
        item = temp
        while (pos != cycle_start) {
            pos = cycle_start
            i = ((cycle_start + 1).toBigInteger())
            while (i.compareTo((n).toBigInteger()) < 0) {
                if (arr[(i).toInt()]!! < item) {
                    pos = pos + 1
                }
                i = i.add((1).toBigInteger())
            }
            while (item == arr[pos]!!) {
                pos = pos + 1
            }
            var temp2: Int = (arr[pos]!!).toInt()
            _listSet(arr, pos, item)
            item = temp2
        }
        cycle_start = cycle_start + 1
    }
    return arr
}

fun main() {
    println(cycle_sort(mutableListOf(4, 3, 2, 1)).toString())
    println(cycle_sort(mutableListOf(0 - 4, 20, 0, 0 - 50, 100, 0 - 1)).toString())
    println(cycle_sort(mutableListOf<Int>()).toString())
}
