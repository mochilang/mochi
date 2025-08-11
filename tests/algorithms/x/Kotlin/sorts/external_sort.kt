import java.math.BigInteger

fun <T> _listSet(lst: MutableList<T>, idx: Int, v: T) { while (lst.size <= idx) lst.add(v); lst[idx] = v }

fun subarray(xs: MutableList<Int>, start: Int, end: Int): MutableList<Int> {
    var result: MutableList<Int> = mutableListOf<Int>()
    var k: Int = (start).toInt()
    while (k < end) {
        result = run { val _tmp = result.toMutableList(); _tmp.add(xs[k]!!); _tmp }
        k = k + 1
    }
    return result
}

fun merge(left_half: MutableList<Int>, right_half: MutableList<Int>): MutableList<Int> {
    var result: MutableList<Int> = mutableListOf<Int>()
    var i: Int = (0).toInt()
    var j: Int = (0).toInt()
    while ((i < left_half.size) && (j < right_half.size)) {
        if (left_half[i]!! < right_half[j]!!) {
            result = run { val _tmp = result.toMutableList(); _tmp.add(left_half[i]!!); _tmp }
            i = i + 1
        } else {
            result = run { val _tmp = result.toMutableList(); _tmp.add(right_half[j]!!); _tmp }
            j = j + 1
        }
    }
    while (i < left_half.size) {
        result = run { val _tmp = result.toMutableList(); _tmp.add(left_half[i]!!); _tmp }
        i = i + 1
    }
    while (j < right_half.size) {
        result = run { val _tmp = result.toMutableList(); _tmp.add(right_half[j]!!); _tmp }
        j = j + 1
    }
    return result
}

fun merge_sort(array: MutableList<Int>): MutableList<Int> {
    if (array.size <= 1) {
        return array
    }
    var middle: Int = (array.size / 2).toInt()
    var left_half: MutableList<Int> = subarray(array, 0, middle)
    var right_half: MutableList<Int> = subarray(array, middle, array.size)
    var sorted_left: MutableList<Int> = merge_sort(left_half)
    var sorted_right: MutableList<Int> = merge_sort(right_half)
    return merge(sorted_left, sorted_right)
}

fun split_into_blocks(data: MutableList<Int>, block_size: Int): MutableList<MutableList<Int>> {
    var blocks: MutableList<MutableList<Int>> = mutableListOf<MutableList<Int>>()
    var i: Int = (0).toInt()
    while (i < data.size) {
        var end = if ((i + block_size) < data.size) i + block_size else data.size
        var block: MutableList<Int> = subarray(data, i, (end.toInt()))
        var sorted_block: MutableList<Int> = merge_sort(block)
        blocks = run { val _tmp = blocks.toMutableList(); _tmp.add(sorted_block); _tmp }
        i = (end.toInt())
    }
    return blocks
}

fun merge_blocks(blocks: MutableList<MutableList<Int>>): MutableList<Int> {
    var num_blocks: Int = (blocks.size).toInt()
    var indices: MutableList<Int> = mutableListOf<Int>()
    var i: Int = (0).toInt()
    while (i < num_blocks) {
        indices = run { val _tmp = indices.toMutableList(); _tmp.add(0); _tmp }
        i = i + 1
    }
    var result: MutableList<Int> = mutableListOf<Int>()
    var done: Boolean = false
    while (!done) {
        done = true
        var min_val: Int = (0).toInt()
        var min_block: BigInteger = ((0 - 1).toBigInteger())
        var j: Int = (0).toInt()
        while (j < num_blocks) {
            var idx: Int = (indices[j]!!).toInt()
            if (idx < (blocks[j]!!).size) {
                var _val: Int = ((((blocks[j]!!) as MutableList<Int>))[idx]!!).toInt()
                if ((min_block.compareTo((0 - 1).toBigInteger()) == 0) || (_val < min_val)) {
                    min_val = _val
                    min_block = (j.toBigInteger())
                }
                done = false
            }
            j = j + 1
        }
        if (!done) {
            result = run { val _tmp = result.toMutableList(); _tmp.add(min_val); _tmp }
            _listSet(indices, (min_block).toInt(), indices[(min_block).toInt()]!! + 1)
        }
    }
    return result
}

fun external_sort(data: MutableList<Int>, block_size: Int): MutableList<Int> {
    var blocks: MutableList<MutableList<Int>> = split_into_blocks(data, block_size)
    return merge_blocks(blocks)
}

fun user_main(): Unit {
    var data: MutableList<Int> = mutableListOf(7, 1, 5, 3, 9, 2, 6, 4, 8, 0)
    var sorted_data: MutableList<Int> = external_sort(data, 3)
    println(sorted_data)
}

fun main() {
    user_main()
}
