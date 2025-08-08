fun <T> _listSet(lst: MutableList<T>, idx: Int, v: T) { while (lst.size <= idx) lst.add(v); lst[idx] = v }

var arr: MutableList<Int> = mutableListOf(0, 0, 0, 0, 0)
fun zeros(n: Int): MutableList<Int> {
    var res: MutableList<Int> = mutableListOf<Int>()
    var i: Int = (0).toInt()
    while (i < n) {
        res = run { val _tmp = res.toMutableList(); _tmp.add(0); _tmp }
        i = i + 1
    }
    return res
}

fun update(arr: MutableList<Int>, idx: Int, value: Int): Unit {
    _listSet(arr, idx, value)
}

fun query(arr: MutableList<Int>, left: Int, right: Int): Int {
    var result: Int = (0).toInt()
    var i: Int = (left).toInt()
    while (i < right) {
        if (arr[i]!! > result) {
            result = arr[i]!!
        }
        i = i + 1
    }
    return result
}

fun main() {
    println(query(arr, 0, 5))
    update(arr, 4, 100)
    println(query(arr, 0, 5))
    update(arr, 4, 0)
    update(arr, 2, 20)
    println(query(arr, 0, 5))
    update(arr, 4, 10)
    println(query(arr, 2, 5))
    println(query(arr, 1, 5))
    update(arr, 2, 0)
    println(query(arr, 0, 5))
    arr = zeros(10000)
    update(arr, 255, 30)
    println(query(arr, 0, 10000))
    arr = zeros(6)
    update(arr, 5, 1)
    println(query(arr, 5, 6))
    arr = zeros(6)
    update(arr, 0, 1000)
    println(query(arr, 0, 1))
}
