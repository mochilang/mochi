import java.math.BigInteger

fun <T> _listSet(lst: MutableList<T>, idx: Int, v: T) { while (lst.size <= idx) lst.add(v); lst[idx] = v }

var seed: Int = (1).toInt()
var data: MutableList<Int> = mutableListOf(3, 2, 1)
fun rand(): Int {
    seed = (((Math.floorMod((((seed * 1103515245) + 12345).toLong()), 2147483648L)).toInt())).toInt()
    return seed
}

fun rand_range(max: Int): Int {
    return Math.floorMod(rand(), max)
}

fun shuffle(list_int: MutableList<Int>): MutableList<Int> {
    var i: BigInteger = ((list_int.size - 1).toBigInteger())
    while (i.compareTo((0).toBigInteger()) > 0) {
        var j: Int = (rand_range(((i.add((1).toBigInteger())).toInt()))).toInt()
        var tmp: Int = (list_int[(i).toInt()]!!).toInt()
        _listSet(list_int, (i).toInt(), list_int[j]!!)
        _listSet(list_int, j, tmp)
        i = i.subtract((1).toBigInteger())
    }
    return list_int
}

fun is_sorted(list_int: MutableList<Int>): Boolean {
    var i: Int = (0).toInt()
    while (i < (list_int.size - 1)) {
        if (list_int[i]!! > list_int[i + 1]!!) {
            return false
        }
        i = i + 1
    }
    return true
}

fun bogo_sort(list_int: MutableList<Int>): MutableList<Int> {
    var res: MutableList<Int> = list_int
    while (!is_sorted(res)) {
        res = shuffle(res)
    }
    return res
}

fun main() {
    println(bogo_sort(data).toString())
}
