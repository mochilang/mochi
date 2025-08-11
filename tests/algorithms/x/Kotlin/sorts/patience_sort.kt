import java.math.BigInteger

fun <T> _listSet(lst: MutableList<T>, idx: Int, v: T) { while (lst.size <= idx) lst.add(v); lst[idx] = v }

fun bisect_left(stacks: MutableList<MutableList<Int>>, value: Int): Int {
    var low: Int = (0).toInt()
    var high: Int = (stacks.size).toInt()
    while (low < high) {
        var mid: Int = ((low + high) / 2).toInt()
        var stack: MutableList<Int> = stacks[mid]!!
        var top_idx: Int = (stack.size - 1).toInt()
        var top: Int = (stack[top_idx]!!).toInt()
        if (top < value) {
            low = mid + 1
        } else {
            high = mid
        }
    }
    return low
}

fun reverse_list(src: MutableList<Int>): MutableList<Int> {
    var res: MutableList<Int> = mutableListOf<Int>()
    var i: BigInteger = ((src.size - 1).toBigInteger())
    while (i.compareTo((0).toBigInteger()) >= 0) {
        res = run { val _tmp = res.toMutableList(); _tmp.add(src[(i).toInt()]!!); _tmp }
        i = i.subtract((1).toBigInteger())
    }
    return res
}

fun patience_sort(collection: MutableList<Int>): MutableList<Int> {
    var stacks: MutableList<MutableList<Int>> = mutableListOf<MutableList<Int>>()
    var i: Int = (0).toInt()
    while (i < collection.size) {
        var element: Int = (collection[i]!!).toInt()
        var idx: Int = (bisect_left(stacks, element)).toInt()
        if (idx != stacks.size) {
            var stack: MutableList<Int> = stacks[idx]!!
            _listSet(stacks, idx, run { val _tmp = stack.toMutableList(); _tmp.add(element); _tmp })
        } else {
            var new_stack: MutableList<Int> = mutableListOf(element)
            stacks = run { val _tmp = stacks.toMutableList(); _tmp.add(new_stack); _tmp }
        }
        i = i + 1
    }
    i = 0
    while (i < stacks.size) {
        _listSet(stacks, i, reverse_list(stacks[i]!!))
        i = i + 1
    }
    var indices: MutableList<Int> = mutableListOf<Int>()
    i = 0
    while (i < stacks.size) {
        indices = run { val _tmp = indices.toMutableList(); _tmp.add(0); _tmp }
        i = i + 1
    }
    var total: Int = (0).toInt()
    i = 0
    while (i < stacks.size) {
        total = total + (stacks[i]!!).size
        i = i + 1
    }
    var result: MutableList<Int> = mutableListOf<Int>()
    var count: Int = (0).toInt()
    while (count < total) {
        var min_val: Int = (0).toInt()
        var min_stack: Int = (0 - 1).toInt()
        var j: Int = (0).toInt()
        while (j < stacks.size) {
            var idx: Int = (indices[j]!!).toInt()
            if (idx < (stacks[j]!!).size) {
                var _val: Int = ((((stacks[j]!!) as MutableList<Int>))[idx]!!).toInt()
                if (min_stack < 0) {
                    min_val = _val
                    min_stack = j
                } else {
                    if (_val < min_val) {
                        min_val = _val
                        min_stack = j
                    }
                }
            }
            j = j + 1
        }
        result = run { val _tmp = result.toMutableList(); _tmp.add(min_val); _tmp }
        _listSet(indices, min_stack, indices[min_stack]!! + 1)
        count = count + 1
    }
    i = 0
    while (i < result.size) {
        _listSet(collection, i, result[i]!!)
        i = i + 1
    }
    return collection
}

fun main() {
    println(patience_sort(mutableListOf(1, 9, 5, 21, 17, 6)).toString())
    println(patience_sort(mutableListOf<Int>()).toString())
    println(patience_sort(mutableListOf(0 - 3, 0 - 17, 0 - 48)).toString())
}
