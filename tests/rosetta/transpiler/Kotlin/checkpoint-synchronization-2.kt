var partList: MutableList<String> = mutableListOf("A", "B", "C", "D")
var nAssemblies: Int = 3
fun lower(ch: String): String {
    var upper: String = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    var lower: String = "abcdefghijklmnopqrstuvwxyz"
    var i: Int = 0
    while (i < upper.length) {
        if (ch == upper.substring(i, i + 1)) {
            return lower.substring(i, i + 1)
        }
        i = i + 1
    }
    return ch
}

fun main() {
    for (cycle in 1 until nAssemblies + 1) {
        println("begin assembly cycle " + cycle.toString())
        var a: String = ""
        for (p in partList) {
            println(p + " worker begins part")
            println((p + " worker completed ") + (p.toLowerCase()).toString())
            a = a + (p.toLowerCase()).toString()
        }
        println(((a + " assembled.  cycle ") + cycle.toString()) + " complete")
    }
}
