import java.math.BigInteger

fun <T> _listSet(lst: MutableList<T>, idx: Int, v: T) { while (lst.size <= idx) lst.add(v); lst[idx] = v }

var RADIX: Int = (10).toInt()
fun make_buckets(): MutableList<MutableList<Int>> {
    var buckets: MutableList<MutableList<Int>> = mutableListOf<MutableList<Int>>()
    var i: Int = (0).toInt()
    while (i < RADIX) {
        buckets = run { val _tmp = buckets.toMutableList(); _tmp.add(mutableListOf<Int>()); _tmp }
        i = i + 1
    }
    return buckets
}

fun max_value(xs: MutableList<Int>): Int {
    var max_val: Int = (xs[0]!!).toInt()
    var i: Int = (1).toInt()
    while (i < xs.size) {
        if (xs[i]!! > max_val) {
            max_val = xs[i]!!
        }
        i = i + 1
    }
    return max_val
}

fun radix_sort(list_of_ints: MutableList<Int>): MutableList<Int> {
    var placement: Int = (1).toInt()
    var max_digit: Int = (max_value(list_of_ints)).toInt()
    while (placement <= max_digit) {
        var buckets: MutableList<MutableList<Int>> = make_buckets()
        var i: Int = (0).toInt()
        while (i < list_of_ints.size) {
            var value: Int = (list_of_ints[i]!!).toInt()
            var tmp: Int = (Math.floorMod((value / placement), RADIX)).toInt()
            _listSet(buckets, tmp, run { val _tmp = (buckets[tmp]!!).toMutableList(); _tmp.add(value); _tmp })
            i = i + 1
        }
        var a: Int = (0).toInt()
        var b: Int = (0).toInt()
        while (b < RADIX) {
            var bucket: MutableList<Int> = buckets[b]!!
            var j: Int = (0).toInt()
            while (j < bucket.size) {
                _listSet(list_of_ints, a, bucket[j]!!)
                a = a + 1
                j = j + 1
            }
            b = b + 1
        }
        placement = placement * RADIX
    }
    return list_of_ints
}

fun main() {
    println(radix_sort(mutableListOf(0, 5, 3, 2, 2)).toString())
    println(radix_sort(mutableListOf(1, 100, 10, 1000)).toString())
    println(radix_sort(mutableListOf(15, 14, 13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0)).toString())
}
