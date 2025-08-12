fun panic(msg: String): Nothing { throw RuntimeException(msg) }

fun combinations(n: Int, k: Int): Int {
    if ((k < 0) || (n < k)) {
        panic("Please enter positive integers for n and k where n >= k")
    }
    var res: Int = (1).toInt()
    var i: Int = (0).toInt()
    while (i < k) {
        res = res * (n - i)
        res = res / (i + 1)
        i = i + 1
    }
    return res
}

fun main() {
    println("The number of five-card hands possible from a standard fifty-two card deck is: " + combinations(52, 5).toString())
    println("")
    println(("If a class of 40 students must be arranged into groups of 4 for group projects, there are " + combinations(40, 4).toString()) + " ways to arrange them.")
    println("")
    println(("If 10 teams are competing in a Formula One race, there are " + combinations(10, 3).toString()) + " ways that first, second and third place can be awarded.")
}
