fun main() {
    val a = 10 - 3
    val b = 2 + 2
    println(a)
    println(if (a == 7) 1 else 0)
    println(if ((b as Number).toDouble() < 5) 1 else 0)
}
