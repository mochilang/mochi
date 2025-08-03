fun mkAdd(a: Int): (Int) -> Int {
    return ({ b: Int -> a + b } as (Int) -> Int)
}

fun mysum(x: Int, y: Int): Int {
    return x + y
}

fun partialSum(x: Int): (Int) -> Int {
    return ({ y: Int -> mysum(x, y) } as (Int) -> Int)
}

fun user_main(): Unit {
    var add2: (Int) -> Int = mkAdd(2)
    var add3: (Int) -> Int = mkAdd(3)
    println((add2(5).toString() + " ") + add3(6).toString())
    var partial: (Int) -> Int = partialSum(13)
    println(partial(5).toString())
}

fun main() {
    user_main()
}
