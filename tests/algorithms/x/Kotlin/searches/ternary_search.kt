var precision: Int = (10).toInt()
fun lin_search(left: Int, right: Int, array: MutableList<Int>, target: Int): Int {
    var i: Int = (left).toInt()
    while (i < right) {
        if (array[i]!! == target) {
            return i
        }
        i = i + 1
    }
    return 0 - 1
}

fun ite_ternary_search(array: MutableList<Int>, target: Int): Int {
    var left: Int = (0).toInt()
    var right: Int = (array.size - 1).toInt()
    while (left <= right) {
        if ((right - left) < precision) {
            var idx: Int = (lin_search(left, right + 1, array, target)).toInt()
            return idx
        }
        var one_third: Int = (left + ((right - left) / 3)).toInt()
        var two_third: Int = (right - ((right - left) / 3)).toInt()
        if (array[one_third]!! == target) {
            return one_third
        }
        if (array[two_third]!! == target) {
            return two_third
        }
        if (target < array[one_third]!!) {
            right = one_third - 1
        } else {
            if (array[two_third]!! < target) {
                left = two_third + 1
            } else {
                left = one_third + 1
                right = two_third - 1
            }
        }
    }
    return 0 - 1
}

fun rec_ternary_search(left: Int, right: Int, array: MutableList<Int>, target: Int): Int {
    if (left <= right) {
        if ((right - left) < precision) {
            var idx: Int = (lin_search(left, right + 1, array, target)).toInt()
            return idx
        }
        var one_third: Int = (left + ((right - left) / 3)).toInt()
        var two_third: Int = (right - ((right - left) / 3)).toInt()
        if (array[one_third]!! == target) {
            return one_third
        }
        if (array[two_third]!! == target) {
            return two_third
        }
        if (target < array[one_third]!!) {
            return rec_ternary_search(left, one_third - 1, array, target)
        }
        if (array[two_third]!! < target) {
            return rec_ternary_search(two_third + 1, right, array, target)
        }
        return rec_ternary_search(one_third + 1, two_third - 1, array, target)
    }
    return 0 - 1
}

fun user_main(): Unit {
    var test_list: MutableList<Int> = mutableListOf(0, 1, 2, 8, 13, 17, 19, 32, 42)
    println(ite_ternary_search(test_list, 3).toString())
    println(ite_ternary_search(test_list, 13).toString())
    println(rec_ternary_search(0, test_list.size - 1, test_list, 3).toString())
    println(rec_ternary_search(0, test_list.size - 1, test_list, 13).toString())
}

fun main() {
    user_main()
}
