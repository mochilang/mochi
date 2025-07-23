fun bigTrim(a: MutableList<Int>): MutableList<Int> {
    var a: MutableList<Int> = a
    var n: Int = a.size
    while ((n > 1) && (a[n - 1] == 0)) {
        a = a.subList(0, n - 1)
        n = n - 1
    }
    return a
}

fun bigFromInt(x: Int): MutableList<Int> {
    if (x == 0) {
        return mutableListOf(0)
    }
    var digits: MutableList<Int> = mutableListOf()
    var n: Int = x
    while (n > 0) {
        digits = run { val _tmp = digits.toMutableList(); _tmp.add(n % 10); _tmp } as MutableList<Int>
        n = n / 10
    }
    return digits
}

fun bigCmp(a: MutableList<Int>, b: MutableList<Int>): Int {
    if (a.size > b.size) {
        return 1
    }
    if (a.size < b.size) {
        return 0 - 1
    }
    var i: Int = (a.size as Int) - 1
    while (i >= 0) {
        if (a[i] > b[i]) {
            return 1
        }
        if (a[i] < b[i]) {
            return 0 - 1
        }
        i = i - 1
    }
    return 0
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

fun bigMulSmall(a: MutableList<Int>, m: Int): MutableList<Int> {
    if (m == 0) {
        return mutableListOf(0)
    }
    var res: MutableList<Int> = mutableListOf()
    var carry: Int = 0
    var i: Int = 0
    while (i < (a.size as Number).toDouble()) {
        var prod: Int = (a[i] * m) + carry
        res = run { val _tmp = res.toMutableList(); _tmp.add(prod % 10); _tmp } as MutableList<Int>
        carry = prod / 10
        i = i + 1
    }
    while (carry > 0) {
        res = run { val _tmp = res.toMutableList(); _tmp.add(carry % 10); _tmp } as MutableList<Int>
        carry = carry / 10
    }
    return bigTrim(res) as MutableList<Int>
}

fun bigMulBig(a: MutableList<Int>, b: MutableList<Int>): MutableList<Int> {
    var res: MutableList<Int> = mutableListOf()
    var i: Int = 0
    while (i < ((a.size as Number).toDouble() + (b.size as Number).toDouble())) {
        res = run { val _tmp = res.toMutableList(); _tmp.add(0); _tmp } as MutableList<Int>
        i = i + 1
    }
    i = 0
    while (i < (a.size as Number).toDouble()) {
        var carry: Int = 0
        var j: Int = 0
        while (j < (b.size as Number).toDouble()) {
            var idx: Int = i + j
            var prod: Int = (res[idx] + (a[i] * b[j])) + carry
            res[idx] = prod % 10
            carry = prod / 10
            j = j + 1
        }
        var idx: Int = i + (b.size as Int)
        while (carry > 0) {
            var prod: Int = res[idx] + carry
            res[idx] = prod % 10
            carry = prod / 10
            idx = idx + 1
        }
        i = i + 1
    }
    return bigTrim(res) as MutableList<Int>
}

fun bigMulPow10(a: MutableList<Int>, k: Int): MutableList<Int> {
    var a: MutableList<Int> = a
    var i: Int = 0
    while (i < k) {
        a = (mutableListOf(0) + a).toMutableList()
        i = i + 1
    }
    return a
}

fun bigDivSmall(a: MutableList<Int>, m: Int): MutableList<Int> {
    var res: MutableList<Int> = mutableListOf()
    var rem: Int = 0
    var i: Int = (a.size as Int) - 1
    while (i >= 0) {
        var cur: Int = (rem * 10) + a[i]
        var q: Int = cur / m
        rem = cur % m
        res = (mutableListOf(q) + res).toMutableList()
        i = i - 1
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
    return s
}

fun repeat(ch: String, n: Int): String {
    var s: String = ""
    var i: Int = 0
    while (i < n) {
        s = s + ch
        i = i + 1
    }
    return s
}

fun sortInts(xs: MutableList<Int>): MutableList<Int> {
    var res: MutableList<Int> = mutableListOf()
    var tmp: MutableList<Int> = xs
    while ((tmp.size as Number).toDouble() > 0) {
        var min: Int = tmp[0]
        var idx: Int = 0
        var i: Int = 1
        while (i < (tmp.size as Number).toDouble()) {
            if (tmp[i] < min) {
                min = tmp[i]
                idx = i
            }
            i = i + 1
        }
        res = (res + mutableListOf(min)).toMutableList()
        var out: MutableList<Int> = mutableListOf()
        var j: Int = 0
        while (j < (tmp.size as Number).toDouble()) {
            if (j != idx) {
                out = (out + mutableListOf(tmp[j])).toMutableList()
            }
            j = j + 1
        }
        tmp = out
    }
    return res
}

fun primesUpTo(n: Int): MutableList<Int> {
    var sieve: MutableList<Boolean> = mutableListOf()
    var i: Int = 0
    while (i <= n) {
        sieve = run { val _tmp = sieve.toMutableList(); _tmp.add(true); _tmp } as MutableList<Boolean>
        i = i + 1
    }
    var p: Int = 2
    while ((p * p) <= n) {
        if (sieve[p] as Boolean) {
            var m: Int = p * p
            while (m <= n) {
                sieve[m] = false
                m = m + p
            }
        }
        p = p + 1
    }
    var res: MutableList<Int> = mutableListOf()
    var x: Int = 2
    while (x <= n) {
        if (sieve[x] as Boolean) {
            res = run { val _tmp = res.toMutableList(); _tmp.add(x); _tmp } as MutableList<Int>
        }
        x = x + 1
    }
    return res
}

fun factorialExp(n: Int, primes: MutableList<Int>): MutableMap<String, Int> {
    var m: MutableMap<String, Int> = mutableMapOf<Any, Any>() as MutableMap<String, Int>
    for (p in primes) {
        if (p > n) {
            break
        }
        var t: Int = n
        var e: Int = 0
        while (t > 0) {
            t = t / p
            e = e + t
        }
        (m)[p.toString()] = e
    }
    return m
}

fun factorSmall(x: Int, primes: MutableList<Int>): MutableMap<String, Int> {
    var f: MutableMap<String, Int> = mutableMapOf<Any, Any>() as MutableMap<String, Int>
    var n: Int = x
    for (p in primes) {
        if ((p * p) > n) {
            break
        }
        var c: Int = 0
        while ((n % p) == 0) {
            c = c + 1
            n = n / p
        }
        if (c > 0) {
            (f)[p.toString()] = c
        }
    }
    if (n > 1) {
        (f)[n.toString()] = ((f)["get"]!!(n.toString(), 0) as Int) + 1
    }
    return f
}

fun computeIP(n: Int, primes: MutableList<Int>): MutableList<Int> {
    var exps: MutableMap<String, Int> = factorialExp(6 * n, primes) as MutableMap<String, Int>
    val fn: MutableMap<String, Int> = factorialExp(n, primes) as MutableMap<String, Int>
    for (k in fn.keys) {
        (exps)[k] = ((exps)["get"]!!(k, 0) as Int) - (6 * (fn)[k] as Int)
    }
    (exps)["2"] = ((exps)["get"]!!("2", 0) as Int) + 5
    val t2: Int = (((532 * n) * n) + (126 * n)) + 9
    val ft2: MutableMap<String, Int> = factorSmall(t2, primes) as MutableMap<String, Int>
    for (k in ft2.keys) {
        (exps)[k] = ((exps)["get"]!!(k, 0) as Int) + (ft2)[k] as Int
    }
    (exps)["3"] = ((exps)["get"]!!("3", 0) as Int) - 1
    var keys: MutableList<Int> = mutableListOf()
    for (k in exps.keys) {
        keys = run { val _tmp = keys.toMutableList(); _tmp.add(k.toInt()); _tmp } as MutableList<Int>
    }
    keys = sortInts(keys) as MutableList<Int>
    var res: MutableList<Int> = bigFromInt(1) as MutableList<Int>
    for (p in keys) {
        var e: Int = (exps)[p.toString()] as Int
        var i: Int = 0
        while (i < e) {
            res = bigMulSmall(res, p) as MutableList<Int>
            i = i + 1
        }
    }
    return res
}

fun formatTerm(ip: MutableList<Int>, pw: Int): String {
    var s: String = bigToString(ip) as String
    if (pw >= s.length) {
        var frac: String = repeat("0", pw - s.length) as String + s
        if (frac.length < 33) {
            frac = frac + repeat("0", 33 - frac.length) as String
        }
        return "0." + (frac.substring(0, 33)).toString()
    }
    var intpart: String = s.substring(0, s.length - pw)
    var frac: String = s.substring(s.length - pw, s.length)
    if (frac.length < 33) {
        frac = frac + repeat("0", 33 - frac.length) as String
    }
    return (intpart + ".") + (frac.substring(0, 33)).toString()
}

fun bigAbsDiff(a: MutableList<Int>, b: MutableList<Int>): MutableList<Int> {
    if (bigCmp(a, b) as Int >= 0) {
        return bigSub(a, b) as MutableList<Int>
    }
    return bigSub(b, a) as MutableList<Int>
}

fun user_main(): Unit {
    val primes: MutableList<Int> = primesUpTo(2000) as MutableList<Int>
    println("N                               Integer Portion  Pow  Nth Term (33 dp)")
    val line: String = repeat("-", 89) as String
    println(line)
    var sum: MutableList<Int> = bigFromInt(0) as MutableList<Int>
    var prev: MutableList<Int> = bigFromInt(0) as MutableList<Int>
    var denomPow: Int = 0
    var n: Int = 0
    while (true) {
        val ip: MutableList<Int> = computeIP(n, primes) as MutableList<Int>
        val pw: Int = (6 * n) + 3
        if (pw > denomPow) {
            sum = bigMulPow10(sum, pw - denomPow) as MutableList<Int>
            prev = bigMulPow10(prev, pw - denomPow) as MutableList<Int>
            denomPow = pw
        }
        if (n < 10) {
            val termStr: String = formatTerm(ip, pw) as String
            var ipStr: String = bigToString(ip) as String
            while (ipStr.length < 44) {
                ipStr = " " + ipStr
            }
            var pwStr: String = (0 - pw).toString()
            while (pwStr.length < 3) {
                pwStr = " " + pwStr
            }
            var padTerm: String = termStr
            while (padTerm.length < 35) {
                padTerm = padTerm + " "
            }
            println((((((n.toString() + "  ") + ipStr) + "  ") + pwStr) + "  ") + padTerm)
        }
        sum = bigAdd(sum, ip) as MutableList<Int>
        val diff: MutableList<Int> = bigAbsDiff(sum, prev) as MutableList<Int>
        if ((denomPow >= 70) && (bigCmp(diff, bigMulPow10(bigFromInt(1) as MutableList<Int>, denomPow - 70) as MutableList<Int>) as Int < 0)) {
            break
        }
        prev = sum
        n = n + 1
    }
    val precision: Int = 70
    val target: MutableList<Int> = bigMulPow10(bigFromInt(1) as MutableList<Int>, denomPow + (2 * precision)) as MutableList<Int>
    var low: MutableList<Int> = bigFromInt(0) as MutableList<Int>
    var high: MutableList<Int> = bigMulPow10(bigFromInt(1) as MutableList<Int>, precision + 1) as MutableList<Int>
    while (bigCmp(low, bigSub(high, bigFromInt(1) as MutableList<Int>) as MutableList<Int>) as Int < 0) {
        var mid: MutableList<Int> = bigDivSmall(bigAdd(low, high) as MutableList<Int>, 2) as MutableList<Int>
        var prod: MutableList<Int> = bigMulBig(bigMulBig(mid, mid) as MutableList<Int>, sum) as MutableList<Int>
        if (bigCmp(prod, target) as Int <= 0) {
            low = mid
        } else {
            high = bigSub(mid, bigFromInt(1) as MutableList<Int>) as MutableList<Int>
        }
    }
    var piInt: MutableList<Int> = low
    var piStr: String = bigToString(piInt) as String
    if (piStr.length <= precision) {
        piStr = repeat("0", (precision - piStr.length) + 1) as String + piStr
    }
    var out: String = ((piStr.substring(0, piStr.length - precision)).toString() + ".") + (piStr.substring(piStr.length - precision, piStr.length)).toString()
    println("")
    println("Pi to 70 decimal places is:")
    println(out)
}

fun main() {
    user_main()
}
