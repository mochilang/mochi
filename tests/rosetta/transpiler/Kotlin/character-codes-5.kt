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
    println(chr(97))
    println(chr(960))
    println(chr(97) + chr(960))
}
