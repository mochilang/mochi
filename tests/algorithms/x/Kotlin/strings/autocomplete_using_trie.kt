fun _sliceStr(s: String, start: Int, end: Int): String {
    val st = if (start < 0) 0 else start
    val en = if (end > s.length) s.length else end
    return if (st >= en) "" else s.substring(st, en)
}

var words: MutableList<String> = mutableListOf("depart", "detergent", "daring", "dog", "deer", "deal")
fun autocomplete_using_trie(prefix: String): MutableList<String> {
    var result: MutableList<String> = mutableListOf<String>()
    var i: Int = (0).toInt()
    while (i < words.size) {
        var w: String = words[i]!!
        if (_sliceStr(w, 0, prefix.length) == prefix) {
            result = run { val _tmp = result.toMutableList(); _tmp.add(w + " "); _tmp }
        }
        i = i + 1
    }
    return result
}

fun main() {
    println(autocomplete_using_trie("de").toString())
}
