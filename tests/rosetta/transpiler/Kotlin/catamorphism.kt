var n: MutableList<Int> = mutableListOf(1, 2, 3, 4, 5)
fun add(a: Int, b: Int): Int {
    return a + b
}

fun sub(a: Int, b: Int): Int {
    return a - b
}

fun mul(a: Int, b: Int): Int {
    return a * b
}

fun fold(f: (Int, Int) -> Int, xs: MutableList<Int>): Int {
    var r: Int = xs[0]!!
    var i: Int = 1
    while (i < xs.size) {
        r = ((f(r, xs[i]!!)) as Int)
        i = i + 1
    }
    return r
}

fun main() {
    println(fold(({ a: Int, b: Int -> add(a, b) } as (Int, Int) -> Int), n))
    println(fold(({ a: Int, b: Int -> sub(a, b) } as (Int, Int) -> Int), n))
    println(fold(({ a: Int, b: Int -> mul(a, b) } as (Int, Int) -> Int), n))
}
