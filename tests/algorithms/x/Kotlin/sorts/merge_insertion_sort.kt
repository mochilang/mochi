import java.math.BigInteger

fun binary_search_insertion_from(sorted_list: MutableList<Int>, item: Int, start: Int): MutableList<Int> {
    var left: Int = (start).toInt()
    var right: BigInteger = ((sorted_list.size - 1).toBigInteger())
    while ((left).toBigInteger().compareTo((right)) <= 0) {
        var middle = ((left).toBigInteger().add((right))).divide((2).toBigInteger())
        if ((left).toBigInteger().compareTo((right)) == 0) {
            if (sorted_list[(middle).toInt()]!! < item) {
                left = ((middle.add((1).toBigInteger())).toInt())
            }
            break
        } else {
            if (sorted_list[(middle).toInt()]!! < item) {
                left = ((middle.add((1).toBigInteger())).toInt())
            } else {
                right = middle.subtract((1).toBigInteger())
            }
        }
    }
    var result: MutableList<Int> = mutableListOf<Int>()
    var i: Int = (0).toInt()
    while (i < left) {
        result = run { val _tmp = result.toMutableList(); _tmp.add(sorted_list[i]!!); _tmp }
        i = i + 1
    }
    result = run { val _tmp = result.toMutableList(); _tmp.add(item); _tmp }
    while (i < sorted_list.size) {
        result = run { val _tmp = result.toMutableList(); _tmp.add(sorted_list[i]!!); _tmp }
        i = i + 1
    }
    return result
}

fun binary_search_insertion(sorted_list: MutableList<Int>, item: Int): MutableList<Int> {
    return binary_search_insertion_from(sorted_list, item, 0)
}

fun merge(left: MutableList<MutableList<Int>>, right: MutableList<MutableList<Int>>): MutableList<MutableList<Int>> {
    var result: MutableList<MutableList<Int>> = mutableListOf<MutableList<Int>>()
    var i: Int = (0).toInt()
    var j: Int = (0).toInt()
    while ((i < left.size) && (j < right.size)) {
        if ((((left[i]!!) as MutableList<Int>))[0]!! < (((right[j]!!) as MutableList<Int>))[0]!!) {
            result = run { val _tmp = result.toMutableList(); _tmp.add(left[i]!!); _tmp }
            i = i + 1
        } else {
            result = run { val _tmp = result.toMutableList(); _tmp.add(right[j]!!); _tmp }
            j = j + 1
        }
    }
    while (i < left.size) {
        result = run { val _tmp = result.toMutableList(); _tmp.add(left[i]!!); _tmp }
        i = i + 1
    }
    while (j < right.size) {
        result = run { val _tmp = result.toMutableList(); _tmp.add(right[j]!!); _tmp }
        j = j + 1
    }
    return result
}

fun sortlist_2d(list_2d: MutableList<MutableList<Int>>): MutableList<MutableList<Int>> {
    var length: Int = (list_2d.size).toInt()
    if (length <= 1) {
        return list_2d
    }
    var middle: Int = (length / 2).toInt()
    var left: MutableList<MutableList<Int>> = mutableListOf<MutableList<Int>>()
    var i: Int = (0).toInt()
    while (i < middle) {
        left = run { val _tmp = left.toMutableList(); _tmp.add(list_2d[i]!!); _tmp }
        i = i + 1
    }
    var right: MutableList<MutableList<Int>> = mutableListOf<MutableList<Int>>()
    var j: Int = (middle).toInt()
    while (j < length) {
        right = run { val _tmp = right.toMutableList(); _tmp.add(list_2d[j]!!); _tmp }
        j = j + 1
    }
    return merge(sortlist_2d(left), sortlist_2d(right))
}

fun merge_insertion_sort(collection: MutableList<Int>): MutableList<Int> {
    if (collection.size <= 1) {
        return collection
    }
    var two_paired_list: MutableList<MutableList<Int>> = mutableListOf<MutableList<Int>>()
    var has_last_odd_item: Boolean = false
    var i: Int = (0).toInt()
    while (i < collection.size) {
        if (i == (collection.size - 1)) {
            has_last_odd_item = true
        } else {
            var a: Int = (collection[i]!!).toInt()
            var b: Int = (collection[i + 1]!!).toInt()
            if (a < b) {
                two_paired_list = run { val _tmp = two_paired_list.toMutableList(); _tmp.add(mutableListOf(a, b)); _tmp }
            } else {
                two_paired_list = run { val _tmp = two_paired_list.toMutableList(); _tmp.add(mutableListOf(b, a)); _tmp }
            }
        }
        i = i + 2
    }
    var sorted_list_2d: MutableList<MutableList<Int>> = sortlist_2d(two_paired_list)
    var result: MutableList<Int> = mutableListOf<Int>()
    i = 0
    while (i < sorted_list_2d.size) {
        result = run { val _tmp = result.toMutableList(); _tmp.add((((sorted_list_2d[i]!!) as MutableList<Int>))[0]!!); _tmp }
        i = i + 1
    }
    result = run { val _tmp = result.toMutableList(); _tmp.add((((sorted_list_2d[sorted_list_2d.size - 1]!!) as MutableList<Int>))[1]!!); _tmp }
    if ((has_last_odd_item as Boolean)) {
        result = binary_search_insertion(result, collection[collection.size - 1]!!)
    }
    var inserted_before: Boolean = false
    var idx: Int = (0).toInt()
    while (idx < (sorted_list_2d.size - 1)) {
        if (has_last_odd_item && (result[idx]!! == collection[collection.size - 1]!!)) {
            inserted_before = true
        }
        var pivot: Int = ((((sorted_list_2d[idx]!!) as MutableList<Int>))[1]!!).toInt()
        if ((inserted_before as Boolean)) {
            result = binary_search_insertion_from(result, pivot, idx + 2)
        } else {
            result = binary_search_insertion_from(result, pivot, idx + 1)
        }
        idx = idx + 1
    }
    return result
}

fun user_main(): Unit {
    var example1: MutableList<Int> = mutableListOf(0, 5, 3, 2, 2)
    var example2: MutableList<Int> = mutableListOf(99)
    var example3: MutableList<Int> = mutableListOf(0 - 2, 0 - 5, 0 - 45)
    println(merge_insertion_sort(example1).toString())
    println(merge_insertion_sort(example2).toString())
    println(merge_insertion_sort(example3).toString())
}

fun main() {
    user_main()
}
