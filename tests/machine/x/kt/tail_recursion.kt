tailrec fun sumRec(n: Int, acc: Int): Int {
    return if (n == 0) acc else sumRec(n - 1, acc + n)
}

fun main() {
    println(sumRec(10, 0))
}
