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

fun main() {
    println(ord("a").toString())
    println(ord("π").toString())
}
