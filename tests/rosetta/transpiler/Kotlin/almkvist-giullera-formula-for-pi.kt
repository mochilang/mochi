import java.math.BigInteger

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
    var i: BigInteger = a.size - 1
    while ((i as Number).toDouble() >= 0) {
        if ((a)[i] as Int > (b)[i] as Int) {
            return 1
        }
        if ((a)[i] as Int < (b)[i] as Int) {
            return 0 - 1
        }
        i = (i as Int) - 1
    }
    return 0
}

fun bigAdd(a: MutableList<Int>, b: MutableList<Int>): MutableList<Int> {
    var res: MutableList<Int> = mutableListOf()
    var carry: Int = 0
    var i: Int = 0
    while ((((i < a.size) || (i < b.size) as Boolean)) || (carry > 0)) {
        var av: Int = 0
        if (i < a.size) {
            av = a[i]
        }
        var bv: Int = 0
        if (i < b.size) {
            bv = b[i]
        }
        var s: BigInteger = (av + bv) + carry
        res = run { val _tmp = res.toMutableList(); _tmp.add((s as Int) % 10); _tmp } as MutableList<Int>
        carry = (s as Int) / 10
        i = i + 1
    }
    return bigTrim(res)
}

fun bigSub(a: MutableList<Int>, b: MutableList<Int>): MutableList<Int> {
    var res: MutableList<Int> = mutableListOf()
    var borrow: Int = 0
    var i: Int = 0
    while (i < a.size) {
        var av: Int = a[i]
        var bv: Int = 0
        if (i < b.size) {
            bv = b[i]
        }
        var diff: BigInteger = (av - bv) - borrow
        if ((diff as Number).toDouble() < 0) {
            diff = (diff as Int) + 10
            borrow = 1
        } else {
            borrow = 0
        }
        res = run { val _tmp = res.toMutableList(); _tmp.add(diff); _tmp } as MutableList<Int>
        i = i + 1
    }
    return bigTrim(res)
}

fun bigMulSmall(a: MutableList<Int>, m: Int): MutableList<Int> {
    if (m == 0) {
        return mutableListOf(0)
    }
    var res: MutableList<Int> = mutableListOf()
    var carry: Int = 0
    var i: Int = 0
    while (i < a.size) {
        var prod: BigInteger = (a[i] * m) + carry
        res = run { val _tmp = res.toMutableList(); _tmp.add((prod as Int) % 10); _tmp } as MutableList<Int>
        carry = (prod as Int) / 10
        i = i + 1
    }
    while (carry > 0) {
        res = run { val _tmp = res.toMutableList(); _tmp.add(carry % 10); _tmp } as MutableList<Int>
        carry = carry / 10
    }
    return bigTrim(res)
}

fun bigMulBig(a: MutableList<Int>, b: MutableList<Int>): MutableList<Int> {
    var res: MutableList<Int> = mutableListOf()
    var i: Int = 0
    while (i < (a.size + b.size)) {
        res = run { val _tmp = res.toMutableList(); _tmp.add(0); _tmp } as MutableList<Int>
        i = i + 1
    }
    i = 0
    while (i < a.size) {
        var carry: Int = 0
        var j: Int = 0
        while (j < b.size) {
            var idx: BigInteger = i + j
            var prod: BigInteger = ((res)[idx] as Int + (a[i] * b[j])) + carry
            res[idx] = (prod as Int) % 10
            carry = (prod as Int) / 10
            j = j + 1
        }
        var idx: BigInteger = i + b.size
        while (carry > 0) {
            var prod: BigInteger = (res)[idx] as Int + carry
            res[idx] = (prod as Int) % 10
            carry = (prod as Int) / 10
            idx = (idx as Int) + 1
        }
        i = i + 1
    }
    return bigTrim(res)
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
    var i: BigInteger = a.size - 1
    while ((i as Number).toDouble() >= 0) {
        var cur: BigInteger = (rem * 10) + (a)[i] as Int
        var q: Int = (cur as Int) / m
        rem = (cur as Int) % m
        res = (mutableListOf(q) + res).toMutableList()
        i = (i as Int) - 1
    }
    return bigTrim(res)
}

fun bigToString(a: MutableList<Int>): String {
    var s: String = ""
    var i: BigInteger = a.size - 1
    while ((i as Number).toDouble() >= 0) {
        s = s + ((a)[i] as Int).toString()
        i = (i as Int) - 1
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
    while (tmp.size > 0) {
        var min: Int = tmp[0]
        var idx: Int = 0
        var i: Int = 1
        while (i < tmp.size) {
            if (tmp[i] < min) {
                min = tmp[i]
                idx = i
            }
            i = i + 1
        }
        res = (res + mutableListOf(min)).toMutableList()
        var out: MutableList<Int> = mutableListOf()
        var j: Int = 0
        while (j < tmp.size) {
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
            var m: BigInteger = p * p
            while ((m as Number).toDouble() <= n) {
                sieve[m] = false
                m = (m as Int) + p
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
    var m: MutableMap<String, Int> = mutableMapOf<Any?, Any?>() as MutableMap<String, Int>
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
    var f: MutableMap<String, Int> = mutableMapOf<Any?, Any?>() as MutableMap<String, Int>
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
        (f)[n.toString()] = (f)["get"]!!(n.toString(), 0) + 1
    }
    return f
}

fun computeIP(n: Int, primes: MutableList<Int>): MutableList<Int> {
    var exps: MutableMap<String, Int> = factorialExp(6 * n, primes)
    val fn: MutableMap<String, Int> = factorialExp(n, primes)
    for (k in fn.keys) {
        (exps)[k] = (exps)["get"]!!(k, 0) - (6 * (fn)[k] as Int)
    }
    (exps)["2"] = (exps)["get"]!!("2", 0) + 5
    val t2: BigInteger = (((532 * n) * n) + (126 * n)) + 9
    val ft2: MutableMap<String, Int> = factorSmall(t2 as Int, primes)
    for (k in ft2.keys) {
        (exps)[k] = (exps)["get"]!!(k, 0) + (ft2)[k] as Int
    }
    (exps)["3"] = (exps)["get"]!!("3", 0) - 1
    var keys: MutableList<Int> = mutableListOf()
    for (k in exps.keys) {
        keys = run { val _tmp = keys.toMutableList(); _tmp.add(k.toInt()); _tmp } as MutableList<Int>
    }
    keys = sortInts(keys)
    var res: MutableList<Int> = bigFromInt(1)
    for (p in keys) {
        var e: Int = (exps)[p.toString()] as Int
        var i: Int = 0
        while (i < e) {
            res = bigMulSmall(res, p)
            i = i + 1
        }
    }
    return res
}

fun formatTerm(ip: MutableList<Int>, pw: Int): String {
    var s: String = bigToString(ip)
    if (pw >= s.length) {
        var frac: String = repeat("0", pw - s.length) + s
        if (frac.length < 33) {
            frac = frac + repeat("0", 33 - frac.length)
        }
        return "0." + (frac.substring(0, 33)).toString()
    }
    var intpart: String = s.substring(0, s.length - pw)
    var frac: String = s.substring(s.length - pw, s.length)
    if (frac.length < 33) {
        frac = frac + repeat("0", 33 - frac.length)
    }
    return (intpart + ".") + (frac.substring(0, 33)).toString()
}

fun bigAbsDiff(a: MutableList<Int>, b: MutableList<Int>): MutableList<Int> {
    if (bigCmp(a, b) >= 0) {
        return bigSub(a, b)
    }
    return bigSub(b, a)
}

fun user_main(): Unit {
    val primes: MutableList<Int> = primesUpTo(2000)
    println("N                               Integer Portion  Pow  Nth Term (33 dp)")
    val line: String = repeat("-", 89)
    println(line)
    var sum: MutableList<Int> = bigFromInt(0)
    var prev: MutableList<Int> = bigFromInt(0)
    var denomPow: Int = 0
    var n: Int = 0
    while (true) {
        val ip: MutableList<Int> = computeIP(n, primes)
        val pw: BigInteger = (6 * n) + 3
        if (pw.compareTo(denomPow.toBigInteger()) > 0) {
            sum = bigMulPow10(sum, pw.subtract(denomPow.toBigInteger()) as Int)
            prev = bigMulPow10(prev, pw.subtract(denomPow.toBigInteger()) as Int)
            denomPow = pw as Int
        }
        if (n < 10) {
            val termStr: String = formatTerm(ip, pw as Int)
            var ipStr: String = bigToString(ip)
            while (ipStr.length < 44) {
                ipStr = " " + ipStr
            }
            var pwStr: String = (0.toBigInteger().subtract(pw)).toString()
            while (pwStr.length < 3) {
                pwStr = " " + pwStr
            }
            var padTerm: String = termStr
            while (padTerm.length < 35) {
                padTerm = padTerm + " "
            }
            println((((((n.toString() + "  ") + ipStr) + "  ") + pwStr) + "  ") + padTerm)
        }
        sum = bigAdd(sum, ip)
        val diff: MutableList<Int> = bigAbsDiff(sum, prev)
        if ((denomPow >= 70) && (bigCmp(diff, bigMulPow10(bigFromInt(1), denomPow - 70)) < 0)) {
            break
        }
        prev = sum
        n = n + 1
    }
    val precision: Int = 70
    val target: MutableList<Int> = bigMulPow10(bigFromInt(1), denomPow + (2 * precision))
    var low: MutableList<Int> = bigFromInt(0)
    var high: MutableList<Int> = bigMulPow10(bigFromInt(1), precision + 1)
    while (bigCmp(low, bigSub(high, bigFromInt(1))) < 0) {
        var mid: MutableList<Int> = bigDivSmall(bigAdd(low, high), 2)
        var prod: MutableList<Int> = bigMulBig(bigMulBig(mid, mid), sum)
        if (bigCmp(prod, target) <= 0) {
            low = mid
        } else {
            high = bigSub(mid, bigFromInt(1))
        }
    }
    var piInt: MutableList<Int> = low
    var piStr: String = bigToString(piInt)
    if (piStr.length <= precision) {
        piStr = repeat("0", (precision - piStr.length) + 1) + piStr
    }
    var out: String = ((piStr.substring(0, piStr.length - precision)).toString() + ".") + (piStr.substring(piStr.length - precision, piStr.length)).toString()
    println("")
    println("Pi to 70 decimal places is:")
    println(out)
}

fun main() {
    user_main()
}
