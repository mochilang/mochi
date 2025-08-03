fun listStr(xs: MutableList<Int>): String {
    var s: String = "["
    var i: Int = 0
    while (i < xs.size) {
        s = s + (xs[i]!!).toString()
        if (i < (xs.size - 1)) {
            s = s + " "
        }
        i = i + 1
    }
    s = s + "]"
    return s
}

fun llStr(lst: MutableList<MutableList<Int>>): String {
    var s: String = "["
    var i: Int = 0
    while (i < lst.size) {
        s = s + listStr(lst[i]!!)
        if (i < (lst.size - 1)) {
            s = s + " "
        }
        i = i + 1
    }
    s = s + "]"
    return s
}

fun concat(a: MutableList<Int>, b: MutableList<Int>): MutableList<Int> {
    var out: MutableList<Int> = mutableListOf<Int>()
    for (v in a) {
        out = run { val _tmp = out.toMutableList(); _tmp.add(v); _tmp }
    }
    for (v in b) {
        out = run { val _tmp = out.toMutableList(); _tmp.add(v); _tmp }
    }
    return out
}

fun cartN(lists: Any?): MutableList<MutableList<Int>> {
    if (lists == null) {
        return mutableListOf<MutableList<Int>>()
    }
    var a: MutableList<MutableList<Int>> = (lists as MutableList<MutableList<Int>>)
    if (a.size == 0) {
        return mutableListOf<MutableList<Int>>(mutableListOf<Int>())
    }
    var out: MutableList<MutableList<Int>> = mutableListOf<MutableList<Int>>()
    var rest: MutableList<MutableList<Int>> = cartN((a.subList(1, a.size) as Any?))
    for (x in a[0]!!) {
        for (p in rest) {
            out = run { val _tmp = out.toMutableList(); _tmp.add(concat(mutableListOf<Int>((x.toInt())), p)); _tmp }
        }
    }
    return out
}

fun user_main(): Unit {
    println(llStr(cartN((mutableListOf(mutableListOf(1, 2), mutableListOf(3, 4)) as Any?))))
    println(llStr(cartN((mutableListOf(mutableListOf(3, 4), mutableListOf(1, 2)) as Any?))))
    println(llStr(cartN((mutableListOf(mutableListOf(1, 2), mutableListOf<Int>()) as Any?))))
    println(llStr(cartN((mutableListOf(mutableListOf<Int>(), mutableListOf(1, 2)) as Any?))))
    println("")
    println("[")
    for (p in cartN((mutableListOf(mutableListOf(1776, 1789), mutableListOf(7, 12), mutableListOf(4, 14, 23), mutableListOf(0, 1)) as Any?))) {
        println(" " + listStr(p))
    }
    println("]")
    println(llStr(cartN((mutableListOf(mutableListOf(1, 2, 3), mutableListOf(30), mutableListOf(500, 100)) as Any?))))
    println(llStr(cartN((mutableListOf(mutableListOf(1, 2, 3), mutableListOf<Int>(), mutableListOf(500, 100)) as Any?))))
    println("")
    println(llStr(cartN((null as Any?))))
    println(llStr(cartN((mutableListOf<Any?>() as Any?))))
}

fun main() {
    user_main()
}
