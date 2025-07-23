var x: Int = 1
fun bigTrim(a: MutableList<Int>): MutableList<Int> {
    var a: MutableList<Int> = a
    var n: Int = a.size
    while ((n > 1) && (a[n - 1] == 0)) {
        a = a.subList(0, n - 1)
        n = n - 1
    }
    return a as MutableList<Int>
}

fun bigFromInt(x: Int): MutableList<Int> {
    if (x == 0) {
        return mutableListOf(0) as MutableList<Int>
    }
    var digits: MutableList<Int> = mutableListOf()
    var n: Int = x
    while (n > 0) {
        digits = run { val _tmp = digits.toMutableList(); _tmp.add(n % 10); _tmp } as MutableList<Int>
        n = n / 10
    }
    return digits as MutableList<Int>
}

fun bigAdd(a: MutableList<Int>, b: MutableList<Int>): MutableList<Int> {
    var res: MutableList<Int> = mutableListOf()
    var carry: Int = 0
    var i: Int = 0
    while ((((i < (a.size as Number).toDouble()) || (i < (b.size as Number).toDouble()) as Boolean)) || (carry > 0)) {
        var av: Int = 0
        if (i < (a.size as Number).toDouble()) {
            av = a[i]
        }
        var bv: Int = 0
        if (i < (b.size as Number).toDouble()) {
            bv = b[i]
        }
        var s: Int = (av + bv) + carry
        res = run { val _tmp = res.toMutableList(); _tmp.add(s % 10); _tmp } as MutableList<Int>
        carry = s / 10
        i = i + 1
    }
    return bigTrim(res) as MutableList<Int>
}

fun bigSub(a: MutableList<Int>, b: MutableList<Int>): MutableList<Int> {
    var res: MutableList<Int> = mutableListOf()
    var borrow: Int = 0
    var i: Int = 0
    while (i < (a.size as Number).toDouble()) {
        var av: Int = a[i]
        var bv: Int = 0
        if (i < (b.size as Number).toDouble()) {
            bv = b[i]
        }
        var diff: Int = (av - bv) - borrow
        if (diff < 0) {
            diff = diff + 10
            borrow = 1
        } else {
            borrow = 0
        }
        res = run { val _tmp = res.toMutableList(); _tmp.add(diff); _tmp } as MutableList<Int>
        i = i + 1
    }
    return bigTrim(res) as MutableList<Int>
}

fun bigToString(a: MutableList<Int>): String {
    var s: String = ""
    var i: Int = (a.size as Int) - 1
    while (i >= 0) {
        s = s + a[i].toString()
        i = i - 1
    }
    return s as String
}

fun minInt(a: Int, b: Int): Int {
    if (a < b) {
        return a as Int
    } else {
        return b as Int
    }
}

fun cumu(n: Int): MutableList<MutableList<Int>> {
    var cache: MutableList<MutableList<MutableList<Int>>> = mutableListOf(mutableListOf(bigFromInt(1)))
    var y: Int = 1
    while (y <= n) {
        var row: MutableList<MutableList<Int>> = mutableListOf(bigFromInt(0))
        var x: Int = 1
        while (x <= y) {
            val _val: MutableList<Int> = (cache[y - x])[minInt(x, y - x)] as MutableList<Int>
            row = run { val _tmp = row.toMutableList(); _tmp.add(bigAdd(row[(row.size as Int) - 1], _val)); _tmp } as MutableList<MutableList<Int>>
            x = x + 1
        }
        cache = run { val _tmp = cache.toMutableList(); _tmp.add(row); _tmp } as MutableList<MutableList<MutableList<Int>>>
        y = y + 1
    }
    return cache[n] as MutableList<MutableList<Int>>
}

fun row(n: Int): MutableList<String> {
    val e: MutableList<MutableList<Int>> = cumu(n)
    var out: MutableList<String> = mutableListOf()
    var i: Int = 0
    while (i < n) {
        val diff: MutableList<Int> = bigSub(e[i + 1] as MutableList<Int>, e[i] as MutableList<Int>)
        out = run { val _tmp = out.toMutableList(); _tmp.add(bigToString(diff)); _tmp } as MutableList<String>
        i = i + 1
    }
    return out as MutableList<String>
}

fun main() {
    println("rows:")
    while (x < 11) {
        val r: MutableList<String> = row(x)
        var line: String = ""
        var i: Int = 0
        while (i < (r.size as Number).toDouble()) {
            line = ((line + " ") + r[i]) + " "
            i = i + 1
        }
        println(line)
        x = x + 1
    }
    println("")
    println("sums:")
    for (num in mutableListOf(23, 123, 1234)) {
        val r = cumu(num)
        println((num.toString() + " ") + (bigToString((r as MutableList<Any?>)[(r.size as Int) - 1]!! as MutableList<Int>)).toString())
    }
}
