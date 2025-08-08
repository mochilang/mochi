data class SuffixTree(var text: String = "")
fun new_suffix_tree(text: String): SuffixTree {
    return SuffixTree(text = text)
}

fun search(tree: SuffixTree, pattern: String): Boolean {
    var n: Int = (tree.text).length
    var m: Int = pattern.length
    if (m == 0) {
        return true
    }
    if (m > n) {
        return false
    }
    var i: Int = 0
    while (i <= (n - m)) {
        if (tree.text.substring(i, i + m) == pattern) {
            return true
        }
        i = i + 1
    }
    return false
}

fun user_main(): Unit {
    var text: String = "monkey banana"
    var suffix_tree: SuffixTree = new_suffix_tree(text)
    var patterns: MutableList<String> = mutableListOf("ana", "ban", "na", "xyz", "mon")
    var i: Int = 0
    while (i < patterns.size) {
        var pattern: String = patterns[i]!!
        var found: Boolean = search(suffix_tree, pattern)
        println((("Pattern '" + pattern) + "' found: ") + found.toString())
        i = i + 1
    }
}

fun main() {
    user_main()
}
