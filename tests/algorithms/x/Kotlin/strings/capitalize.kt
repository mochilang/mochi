var lowercase: String = "abcdefghijklmnopqrstuvwxyz"
var uppercase: String = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
fun index_of(s: String, c: String): Int {
    var i: Int = (0).toInt()
    while (i < s.length) {
        if (s.substring(i, i + 1) == c) {
            return i
        }
        i = i + 1
    }
    return 0 - 1
}

fun capitalize(sentence: String): String {
    if (sentence.length == 0) {
        return ""
    }
    var first: String = sentence.substring(0, 1)
    var idx: Int = (index_of(lowercase, first)).toInt()
    var capital = if (idx >= 0) uppercase.substring(idx, idx + 1) else first
    return (capital).toString() + sentence.substring(1, sentence.length)
}

fun main() {
    println(capitalize("hello world"))
    println(capitalize("123 hello world"))
    println(capitalize(" hello world"))
    println(capitalize("a"))
    println(capitalize(""))
}
