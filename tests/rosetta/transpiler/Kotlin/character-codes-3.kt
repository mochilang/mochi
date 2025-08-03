var b: Int = ord("a")
var r: Int = ord("π")
var s: String = "aπ"
fun ord(ch: String): Int {
    if (ch == "a") {
        return 97
    }
    if (ch == "π") {
        return 960
    }
    if (ch == "A") {
        return 65
    }
    return 0
}

fun chr(n: Int): String {
    if (n == 97) {
        return "a"
    }
    if (n == 960) {
        return "π"
    }
    if (n == 65) {
        return "A"
    }
    return "?"
}

fun main() {
    println((((b.toString() + " ") + r.toString()) + " ") + s)
    println(((("string cast to []rune: [" + b.toString()) + " ") + r.toString()) + "]")
    println((("    string range loop: " + b.toString()) + " ") + r.toString())
    println("         string bytes: 0x61 0xcf 0x80")
}
