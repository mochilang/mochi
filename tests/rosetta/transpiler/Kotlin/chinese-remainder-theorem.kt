var n: MutableList<Int> = mutableListOf(3, 5, 7)
var a: MutableList<Int> = mutableListOf(2, 3, 2)
var res: Int = crt(a, n)
fun egcd(a: Int, b: Int): MutableList<Int> {
    if (a == 0) {
        return mutableListOf(b, 0, 1)
    }
    var res: MutableList<Int> = egcd(Math.floorMod(b, a), a)
    var g: Int = res[0]!!
    var x1: Int = res[1]!!
    var y1: Int = res[2]!!
    return mutableListOf(g, y1 - ((b / a) * x1), x1)
}

fun modInv(a: Int, m: Int): Int {
    var r: MutableList<Int> = egcd(a, m)
    if (r[0]!! != 1) {
        return 0
    }
    var x: Int = r[1]!!
    if (x < 0) {
        return x + m
    }
    return x
}

fun crt(a: MutableList<Int>, n: MutableList<Int>): Int {
    var prod: Int = 1
    var i: Int = 0
    while (i < n.size) {
        prod = prod * n[i]!!
        i = i + 1
    }
    var x: Int = 0
    i = 0
    while (i < n.size) {
        var ni: Int = n[i]!!
        var ai: Int = a[i]!!
        var p: Int = prod / ni
        var inv: Int = modInv(Math.floorMod(p, ni), ni)
        x = x + ((ai * inv) * p)
        i = i + 1
    }
    return Math.floorMod(x, prod)
}

fun main() {
    println(res.toString() + " <nil>")
}
