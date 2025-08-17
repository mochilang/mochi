fun _numToStr(v: Number): String {
    val d = v.toDouble()
    val i = d.toLong()
    return if (d == i.toDouble()) i.toString() else d.toString()
}

fun perfect(n: Int): Boolean {
    if (n <= 0) {
        return false
    }
    var limit: Int = (n / 2).toInt()
    var sum: Int = (0).toInt()
    var i: Int = (1).toInt()
    while (i <= limit) {
        if ((Math.floorMod(n, i)) == 0) {
            sum = sum + i
        }
        i = i + 1
    }
    return sum == n
}

fun user_main(): Unit {
    var numbers: MutableList<Int> = mutableListOf(6, 28, 29, 12, 496, 8128, 0, 0 - 1)
    var idx: Int = (0).toInt()
    while (idx < numbers.size) {
        var num: Int = (numbers[idx]!!).toInt()
        if ((perfect(num)) as Boolean) {
            println(_numToStr(num) + " is a Perfect Number.")
        } else {
            println(_numToStr(num) + " is not a Perfect Number.")
        }
        idx = idx + 1
    }
}

fun main() {
    user_main()
}
