fun <T> _listSet(lst: MutableList<T>, idx: Int, v: T) { while (lst.size <= idx) lst.add(v); lst[idx] = v }

fun odd_even_transposition(xs: MutableList<Int>): MutableList<Int> {
    var arr: MutableList<Int> = xs
    var n: Int = (arr.size).toInt()
    var phase: Int = (0).toInt()
    while (phase < n) {
        var start: Int = ((if ((Math.floorMod(phase, 2)) == 0) 0 else 1 as Int)).toInt()
        var i: Int = (start).toInt()
        while ((i + 1) < n) {
            if (arr[i]!! > arr[i + 1]!!) {
                var tmp: Int = (arr[i]!!).toInt()
                _listSet(arr, i, arr[i + 1]!!)
                _listSet(arr, i + 1, tmp)
            }
            i = i + 2
        }
        phase = phase + 1
    }
    return arr
}

fun user_main(): Unit {
    var data: MutableList<Int> = mutableListOf(10, 9, 8, 7, 6, 5, 4, 3, 2, 1)
    println("Initial List")
    println(data.toString())
    var sorted: MutableList<Int> = odd_even_transposition(data)
    println("Sorted List")
    println(sorted.toString())
}

fun main() {
    user_main()
}
