var vowels: String = "aeiouAEIOU"
fun is_vowel(c: String): Boolean {
    var i: Int = (0).toInt()
    while (i < vowels.length) {
        if (vowels[i].toString() == c) {
            return true
        }
        i = i + 1
    }
    return false
}

fun count_vowels(s: String): Int {
    var count: Int = (0).toInt()
    var i: Int = (0).toInt()
    while (i < s.length) {
        var ch: String = s[i].toString()
        if (((is_vowel(ch)) as Boolean)) {
            count = count + 1
        }
        i = i + 1
    }
    return count
}

fun show(s: String): Unit {
    println(count_vowels(s).toString())
}

fun main() {
    show("hello world")
    show("HELLO WORLD")
    show("123 hello world")
    show("")
    show("a quick brown fox")
    show("the quick BROWN fox")
    show("PYTHON")
}
