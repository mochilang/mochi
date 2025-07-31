fun toBin(n: Int): String {
    if (n == 0) {
        return "0"
    }
    var bits: String = ""
    var x: Int = n
    while (x > 0) {
        bits = (Math.floorMod(x, 2)).toString() + bits
        x = (x / 2).toInt()
    }
    return bits
}

fun main() {
    for (i in 0 until 16) {
        println(toBin(i))
    }
}
