import java.math.BigInteger

fun contains(words: MutableList<String>, target: String): Boolean {
    for (w in words) {
        if (w == target) {
            return true
        }
    }
    return false
}

fun backtrack(s: String, word_dict: MutableList<String>, start: Int): Boolean {
    if (start == s.length) {
        return true
    }
    var end: BigInteger = ((start + 1).toBigInteger())
    while (end.compareTo((s.length).toBigInteger()) <= 0) {
        var substr: String = s.substring(start, (end).toInt())
        if ((word_dict.contains(substr) as Boolean) && backtrack(s, word_dict, (end.toInt()))) {
            return true
        }
        end = end.add((1).toBigInteger())
    }
    return false
}

fun word_break(s: String, word_dict: MutableList<String>): Boolean {
    return backtrack(s, word_dict, 0)
}

fun main() {
    println(word_break("leetcode", mutableListOf("leet", "code")).toString())
    println(word_break("applepenapple", mutableListOf("apple", "pen")).toString())
    println(word_break("catsandog", mutableListOf("cats", "dog", "sand", "and", "cat")).toString())
}
