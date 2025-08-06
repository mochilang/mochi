var alphabet: String = "abcdefghijklmnopqrstuvwxyz"
fun contains(xs: MutableList<String>, x: String): Boolean {
    var i: Int = 0
    while (i < xs.size) {
        if (xs[i]!! == x) {
            return true
        }
        i = i + 1
    }
    return false
}

fun remove_item(xs: MutableList<String>, x: String): MutableList<String> {
    var res: MutableList<String> = mutableListOf<String>()
    var removed: Boolean = false
    var i: Int = 0
    while (i < xs.size) {
        if ((!removed as Boolean) && (xs[i]!! == x)) {
            removed = true
        } else {
            res = run { val _tmp = res.toMutableList(); _tmp.add(xs[i]!!); _tmp }
        }
        i = i + 1
    }
    return res
}

fun word_ladder(current: String, path: MutableList<String>, target: String, words: MutableList<String>): MutableList<String> {
    if (current == target) {
        return path
    }
    var i: Int = 0
    while (i < current.length) {
        var j: Int = 0
        while (j < alphabet.length) {
            var c: String = alphabet.substring(j, j + 1)
            var transformed: String = (current.substring(0, i) + c) + current.substring(i + 1, current.length)
            if (words.contains(transformed)) {
                var new_words: MutableList<String> = remove_item(words, transformed)
                var new_path = run { val _tmp = path.toMutableList(); _tmp.add(transformed); _tmp }
                var result: MutableList<String> = word_ladder(transformed, (new_path as MutableList<String>), target, new_words)
                if (result.size > 0) {
                    return result
                }
            }
            j = j + 1
        }
        i = i + 1
    }
    return mutableListOf<String>()
}

fun user_main(): Unit {
    var w1: MutableList<String> = mutableListOf("hot", "dot", "dog", "lot", "log", "cog")
    println(word_ladder("hit", mutableListOf("hit"), "cog", w1).toString())
    var w2: MutableList<String> = mutableListOf("hot", "dot", "dog", "lot", "log")
    println(word_ladder("hit", mutableListOf("hit"), "cog", w2).toString())
    var w3: MutableList<String> = mutableListOf("load", "goad", "gold", "lead", "lord")
    println(word_ladder("lead", mutableListOf("lead"), "gold", w3).toString())
    var w4: MutableList<String> = mutableListOf("came", "cage", "code", "cade", "gave")
    println(word_ladder("game", mutableListOf("game"), "code", w4).toString())
}

fun main() {
    user_main()
}
