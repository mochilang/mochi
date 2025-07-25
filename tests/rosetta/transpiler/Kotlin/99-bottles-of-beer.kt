fun bottles(n: Int): String {
    if (n == 0) {
        return "No more bottles"
    }
    if (n == 1) {
        return "1 bottle"
    }
    return n.toString() + " bottles"
}

fun user_main(): Unit {
    var i: Int = 99
    while (i > 0) {
        println(bottles(i) + " of beer on the wall")
        println(bottles(i) + " of beer")
        println("Take one down, pass it around")
        println(bottles(i - 1) + " of beer on the wall")
        i = i - 1
    }
}

fun main() {
    user_main()
}
