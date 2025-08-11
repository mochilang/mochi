fun <T> _listSet(lst: MutableList<T>, idx: Int, v: T) { while (lst.size <= idx) lst.add(v); lst[idx] = v }

fun panic(msg: String): Nothing { throw RuntimeException(msg) }

fun dutch_national_flag_sort(seq: MutableList<Int>): MutableList<Int> {
    var a: MutableList<Int> = seq
    var low: Int = (0).toInt()
    var mid: Int = (0).toInt()
    var high: Int = (a.size - 1).toInt()
    while (mid <= high) {
        var v: Int = (a[mid]!!).toInt()
        if (v == 0) {
            var tmp: Int = (a[low]!!).toInt()
            _listSet(a, low, v)
            _listSet(a, mid, tmp)
            low = low + 1
            mid = mid + 1
        } else {
            if (v == 1) {
                mid = mid + 1
            } else {
                if (v == 2) {
                    var tmp2: Int = (a[high]!!).toInt()
                    _listSet(a, high, v)
                    _listSet(a, mid, tmp2)
                    high = high - 1
                } else {
                    panic("The elements inside the sequence must contains only (0, 1, 2) values")
                }
            }
        }
    }
    return a
}

fun main() {
    println(dutch_national_flag_sort(mutableListOf<Int>()))
    println(dutch_national_flag_sort(mutableListOf(0)))
    println(dutch_national_flag_sort(mutableListOf(2, 1, 0, 0, 1, 2)))
    println(dutch_national_flag_sort(mutableListOf(0, 1, 1, 0, 1, 2, 1, 2, 0, 0, 0, 1)))
}
