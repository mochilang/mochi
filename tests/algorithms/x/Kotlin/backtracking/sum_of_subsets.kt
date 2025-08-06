fun sum_list(nums: MutableList<Int>): Int {
    var s: Int = 0
    for (n in nums) {
        s = s + n
    }
    return s
}

fun create_state_space_tree(nums: MutableList<Int>, max_sum: Int, num_index: Int, path: MutableList<Int>, curr_sum: Int, remaining_sum: Int): MutableList<MutableList<Int>> {
    var result: MutableList<MutableList<Int>> = mutableListOf<MutableList<Int>>()
    if ((curr_sum > max_sum) || ((curr_sum + remaining_sum) < max_sum)) {
        return result
    }
    if (curr_sum == max_sum) {
        result = run { val _tmp = result.toMutableList(); _tmp.add(path); _tmp }
        return result
    }
    var index: Int = num_index
    while (index < nums.size) {
        var value: Int = nums[index]!!
        var subres: MutableList<MutableList<Int>> = create_state_space_tree(nums, max_sum, index + 1, run { val _tmp = path.toMutableList(); _tmp.add(value); _tmp }, curr_sum + value, remaining_sum - value)
        var j: Int = 0
        while (j < subres.size) {
            result = run { val _tmp = result.toMutableList(); _tmp.add(subres[j]!!); _tmp }
            j = j + 1
        }
        index = index + 1
    }
    return result
}

fun generate_sum_of_subsets_solutions(nums: MutableList<Int>, max_sum: Int): MutableList<MutableList<Int>> {
    var total: Int = sum_list(nums)
    return create_state_space_tree(nums, max_sum, 0, mutableListOf<Int>(), 0, total)
}

fun user_main(): Unit {
    json(((generate_sum_of_subsets_solutions(mutableListOf(3, 34, 4, 12, 5, 2), 9)) as Any?))
}

fun main() {
    user_main()
}
