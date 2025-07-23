fun amb(wordsets: MutableList<MutableList<String>>, res: MutableList<String>, idx: Int): Boolean {
    if (idx == wordsets.size) {
        return true
    }
    var prev: String = ""
    if (idx > 0) {
        prev = res[idx - 1]
    }
    var i: Int = 0
    while (i < wordsets[idx].size) {
        val w: String = wordsets[idx][i]
        if ((idx == 0) || (prev.substring(prev.length - 1, prev.length) == w.substring(0, 1))) {
            res[idx] = w
            if (amb(wordsets, res, idx + 1) as Boolean as Boolean) {
                return true
            }
        }
        i = i + 1
    }
    return false
}

fun user_main(): Unit {
    val wordset: MutableList<MutableList<String>> = mutableListOf(mutableListOf("the", "that", "a"), mutableListOf("frog", "elephant", "thing"), mutableListOf("walked", "treaded", "grows"), mutableListOf("slowly", "quickly"))
    var res: MutableList<String> = mutableListOf()
    var i: Int = 0
    while (i < wordset.size) {
        res = run { val _tmp = res.toMutableList(); _tmp.add(""); _tmp } as MutableList<String>
        i = i + 1
    }
    if (amb(wordset as MutableList<MutableList<String>>, res, 0) as Boolean as Boolean) {
        var out: String = "[" + res[0]
        var j: Int = 1
        while (j < res.size) {
            out = (out + " ") + res[j]
            j = j + 1
        }
        out = out + "]"
        println(out)
    } else {
        println("No amb found")
    }
}

fun main() {
    user_main()
}
