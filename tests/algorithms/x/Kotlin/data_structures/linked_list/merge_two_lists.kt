import java.math.BigInteger

val _dataDir = "/workspace/mochi/tests/github/TheAlgorithms/Mochi/data_structures/linked_list"

fun <T> _listSet(lst: MutableList<T>, idx: Int, v: T) { while (lst.size <= idx) lst.add(v); lst[idx] = v }

fun _numToStr(v: Number): String {
    val d = v.toDouble()
    val i = d.toLong()
    return if (d == i.toDouble()) i.toString() else d.toString()
}

data class SortedLinkedList(var values: MutableList<Int> = mutableListOf<Int>())
fun sort_list(nums: MutableList<Int>): MutableList<Int> {
    var arr: MutableList<Int> = mutableListOf<Int>()
    var i: Int = (0).toInt()
    while (i < nums.size) {
        arr = run { val _tmp = arr.toMutableList(); _tmp.add(nums[i]!!); _tmp }
        i = i + 1
    }
    var j: Int = (0).toInt()
    while (j < arr.size) {
        var k: Int = (j + 1).toInt()
        while (k < arr.size) {
            if (arr[k]!! < arr[j]!!) {
                var tmp: Int = (arr[j]!!).toInt()
                _listSet(arr, j, arr[k]!!)
                _listSet(arr, k, tmp)
            }
            k = k + 1
        }
        j = j + 1
    }
    return arr
}

fun make_sorted_linked_list(ints: MutableList<Int>): SortedLinkedList {
    return SortedLinkedList(values = sort_list(ints))
}

fun len_sll(sll: SortedLinkedList): Int {
    return (sll.values).size
}

fun str_sll(sll: SortedLinkedList): String {
    var res: String = ""
    var i: Int = (0).toInt()
    while (i < (sll.values).size) {
        res = res + _numToStr((sll.values)[i]!!)
        if ((i + 1) < (sll.values).size) {
            res = res + " -> "
        }
        i = i + 1
    }
    return res
}

fun merge_lists(a: SortedLinkedList, b: SortedLinkedList): SortedLinkedList {
    var combined: MutableList<Int> = mutableListOf<Int>()
    var i: Int = (0).toInt()
    while (i < (a.values).size) {
        combined = run { val _tmp = combined.toMutableList(); _tmp.add((a.values)[i]!!); _tmp }
        i = i + 1
    }
    i = 0
    while (i < (b.values).size) {
        combined = run { val _tmp = combined.toMutableList(); _tmp.add((b.values)[i]!!); _tmp }
        i = i + 1
    }
    return make_sorted_linked_list(combined)
}

fun user_main(): Unit {
    var test_data_odd: MutableList<Int> = mutableListOf(3, 9, 0 - 11, 0, 7, 5, 1, 0 - 1)
    var test_data_even: MutableList<Int> = mutableListOf(4, 6, 2, 0, 8, 10, 3, 0 - 2)
    var sll_one: SortedLinkedList = make_sorted_linked_list(test_data_odd)
    var sll_two: SortedLinkedList = make_sorted_linked_list(test_data_even)
    var merged: SortedLinkedList = merge_lists(sll_one, sll_two)
    println(_numToStr(len_sll(merged)))
    println(str_sll(merged))
}

fun main() {
    user_main()
}
