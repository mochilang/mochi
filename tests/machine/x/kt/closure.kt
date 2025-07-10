fun makeAdder(n: Int): (Int) -> Int {
    return { x -> x + n }
}

fun main() {
    val add10 = makeAdder(10)
    println(add10(7))
}
