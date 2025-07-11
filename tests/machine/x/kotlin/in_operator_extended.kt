data class M(var a: Int)

val xs = mutableListOf(1, 2, 3)

val ys = run {
    val __res = mutableListOf<Int>()
    for (x in xs) {
        if (x % 2 == 1) {
            __res.add(x)
        }
    }
    __res
}

val m = M(a = 1)

val s = "hello"

fun main() {
    println(1 in ys)
    println(2 in ys)
    println("a" in m)
    println("b" in m)
    println("ell" in s)
    println("foo" in s)
}
