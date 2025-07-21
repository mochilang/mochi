fun makeAdder(n: Int): (Int) -> Int {
    return { x: Int -> x + n }
}

fun main() {
    val add10 = makeAdder(10)
    println(add10(7))
}
