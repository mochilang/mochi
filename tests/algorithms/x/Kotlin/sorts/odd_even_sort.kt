fun <T> _listSet(lst: MutableList<T>, idx: Int, v: T) { while (lst.size <= idx) lst.add(v); lst[idx] = v }

fun panic(msg: String): Nothing { throw RuntimeException(msg) }

fun odd_even_sort(xs: MutableList<Int>): MutableList<Int> {
    var arr: MutableList<Int> = mutableListOf<Int>()
    var i: Int = (0).toInt()
    while (i < xs.size) {
        arr = run { val _tmp = arr.toMutableList(); _tmp.add(xs[i]!!); _tmp }
        i = i + 1
    }
    var n: Int = (arr.size).toInt()
    var sorted: Boolean = false
    while (sorted == false) {
        sorted = true
        var j: Int = (0).toInt()
        while (j < (n - 1)) {
            if (arr[j]!! > arr[j + 1]!!) {
                var tmp: Int = (arr[j]!!).toInt()
                _listSet(arr, j, arr[j + 1]!!)
                _listSet(arr, j + 1, tmp)
                sorted = false
            }
            j = j + 2
        }
        j = 1
        while (j < (n - 1)) {
            if (arr[j]!! > arr[j + 1]!!) {
                var tmp: Int = (arr[j]!!).toInt()
                _listSet(arr, j, arr[j + 1]!!)
                _listSet(arr, j + 1, tmp)
                sorted = false
            }
            j = j + 2
        }
    }
    return arr
}

fun print_list(xs: MutableList<Int>): Unit {
    var i: Int = (0).toInt()
    var out: String = ""
    while (i < xs.size) {
        if (i > 0) {
            out = out + " "
        }
        out = out + (xs[i]!!).toString()
        i = i + 1
    }
    println(out)
}

fun test_odd_even_sort(): Unit {
    var a: MutableList<Int> = mutableListOf(5, 4, 3, 2, 1)
    var r1: MutableList<Int> = odd_even_sort(a)
    if ((((((((r1[0]!! != 1) || (r1[1]!! != 2) as Boolean)) || (r1[2]!! != 3) as Boolean)) || (r1[3]!! != 4) as Boolean)) || (r1[4]!! != 5)) {
        panic("case1 failed")
    }
    var b: MutableList<Int> = mutableListOf<Int>()
    var r2: MutableList<Int> = odd_even_sort(b)
    if (r2.size != 0) {
        panic("case2 failed")
    }
    var c: MutableList<Int> = mutableListOf(0 - 10, 0 - 1, 10, 2)
    var r3: MutableList<Int> = odd_even_sort(c)
    if ((((((r3[0]!! != (0 - 10)) || (r3[1]!! != (0 - 1)) as Boolean)) || (r3[2]!! != 2) as Boolean)) || (r3[3]!! != 10)) {
        panic("case3 failed")
    }
    var d: MutableList<Int> = mutableListOf(1, 2, 3, 4)
    var r4: MutableList<Int> = odd_even_sort(d)
    if ((((((r4[0]!! != 1) || (r4[1]!! != 2) as Boolean)) || (r4[2]!! != 3) as Boolean)) || (r4[3]!! != 4)) {
        panic("case4 failed")
    }
}

fun user_main(): Unit {
    test_odd_even_sort()
    var sample: MutableList<Int> = mutableListOf(5, 4, 3, 2, 1)
    var sorted: MutableList<Int> = odd_even_sort(sample)
    print_list(sorted)
}

fun main() {
    user_main()
}
