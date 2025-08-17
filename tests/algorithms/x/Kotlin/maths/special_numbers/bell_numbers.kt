fun panic(msg: String): Nothing { throw RuntimeException(msg) }

fun <T> _listSet(lst: MutableList<T>, idx: Int, v: T) { while (lst.size <= idx) lst.add(v); lst[idx] = v }

fun binomial_coefficient(total_elements: Int, elements_to_choose: Int): Int {
    if ((elements_to_choose == 0) || (elements_to_choose == total_elements)) {
        return 1
    }
    var k: Int = (elements_to_choose).toInt()
    if (k > (total_elements - k)) {
        k = total_elements - k
    }
    var coefficient: Int = (1).toInt()
    var i: Int = (0).toInt()
    while (i < k) {
        coefficient = coefficient * (total_elements - i)
        coefficient = coefficient / (i + 1)
        i = i + 1
    }
    return coefficient
}

fun bell_numbers(max_set_length: Int): MutableList<Int> {
    if (max_set_length < 0) {
        panic("max_set_length must be non-negative")
    }
    var bell: MutableList<Int> = mutableListOf<Int>()
    var i: Int = (0).toInt()
    while (i <= max_set_length) {
        bell = run { val _tmp = bell.toMutableList(); _tmp.add(0); _tmp }
        i = i + 1
    }
    _listSet(bell, 0, 1)
    i = 1
    while (i <= max_set_length) {
        var j: Int = (0).toInt()
        while (j < i) {
            _listSet(bell, i, bell[i]!! + (binomial_coefficient(i - 1, j) * bell[j]!!))
            j = j + 1
        }
        i = i + 1
    }
    return bell
}

fun user_main(): Unit {
    println(bell_numbers(5).toString())
}

fun main() {
    user_main()
}
