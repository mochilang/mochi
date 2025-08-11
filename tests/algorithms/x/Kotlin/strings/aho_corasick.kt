var text: String = "whatever, err ... , wherever"
var keywords: MutableList<String> = mutableListOf("what", "hat", "ver", "er")
fun search_all(text: String, keywords: MutableList<String>): MutableMap<String, MutableList<Int>> {
    var result: MutableMap<String, MutableList<Int>> = mutableMapOf<String, MutableList<Int>>()
    for (word in keywords) {
        var positions: MutableList<Int> = mutableListOf<Int>()
        var m: Int = (word.length).toInt()
        var i: Int = (0).toInt()
        while (i <= (text.length - m)) {
            if (text.substring(i, i + m) == word) {
                positions = run { val _tmp = positions.toMutableList(); _tmp.add(i); _tmp }
            }
            i = i + 1
        }
        if (positions.size > 0) {
            (result)[word] = positions
        }
    }
    return result
}

fun main() {
    println(search_all(text, keywords))
}
