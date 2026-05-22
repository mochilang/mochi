fun main() {
    val xs = mutableListOf(1, 2, 3)
    val ys = run {
    val _res = mutableListOf<Int>()
    for (x in xs) {
        if ((x % 2) == 1) {
            _res.add(x)
        }
    }
    _res
}
    println(1 in ys)
    println(2 in ys)
    val m = mutableMapOf("a" to 1)
    println("a" in m)
    println("b" in m)
    val s = "hello"
    println("ell" in s)
    println("foo" in s)
}
