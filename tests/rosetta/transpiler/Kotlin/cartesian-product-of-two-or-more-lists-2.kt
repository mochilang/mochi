import java.math.BigInteger

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

fun cartN(lists: Any?): MutableList<MutableList<Int>> {
    if (lists == null) {
        return mutableListOf<MutableList<Int>>()
    }
    var a: MutableList<MutableList<Int>> = (lists as MutableList<MutableList<Int>>)
    if (a.size == 0) {
        return mutableListOf<MutableList<Int>>(mutableListOf<Int>())
    }
    var c: Int = 1
    for (xs in a) {
        c = c * xs.size
    }
    if (c == 0) {
        return mutableListOf<MutableList<Int>>()
    }
    var res: MutableList<MutableList<Int>> = mutableListOf<MutableList<Int>>()
    var idx: MutableList<Int> = mutableListOf<Int>()
    for (_u1 in a) {
        idx = run { val _tmp = idx.toMutableList(); _tmp.add(0); _tmp }
    }
    var n: Int = a.size
    var count: Int = 0
    while (count < c) {
        var row: MutableList<Int> = mutableListOf<Int>()
        var j: Int = 0
        while (j < n) {
            row = run { val _tmp = row.toMutableList(); _tmp.add((((a[j]!!) as MutableList<Int>))[idx[j]!!]!!); _tmp }
            j = j + 1
        }
        res = run { val _tmp = res.toMutableList(); _tmp.add(row); _tmp }
        var k: BigInteger = (n - 1).toBigInteger()
        while (k.compareTo((0).toBigInteger()) >= 0) {
            idx[(k).toInt()] = idx[(k).toInt()]!! + 1
            if (idx[(k).toInt()]!! < (a[(k).toInt()]!!).size) {
                break
            }
            idx[(k).toInt()] = 0
            k = k.subtract((1).toBigInteger())
        }
        count = count + 1
    }
    return res
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
