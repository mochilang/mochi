fun indexOf(s: String, ch: String): Int {
    var i: Int = 0
    while (i < s.length) {
        if (s.substring(i, i + 1) == ch) {
            return i
        }
        i = i + 1
    }
    return 0 - 1
}

fun join(xs: MutableList<String>, sep: String): String {
    var res: String = ""
    var i: Int = 0
    while (i < xs.size) {
        if (i > 0) {
            res = res + sep
        }
        res = res + xs[i]!!
        i = i + 1
    }
    return res
}

fun sentenceType(s: String): String {
    if (s.length == 0) {
        return ""
    }
    var types: MutableList<String> = mutableListOf<String>()
    var i: Int = 0
    while (i < s.length) {
        var ch: String = s.substring(i, i + 1)
        if (ch == "?") {
            types = run { val _tmp = types.toMutableList(); _tmp.add("Q"); _tmp }
        } else {
            if (ch == "!") {
                types = run { val _tmp = types.toMutableList(); _tmp.add("E"); _tmp }
            } else {
                if (ch == ".") {
                    types = run { val _tmp = types.toMutableList(); _tmp.add("S"); _tmp }
                }
            }
        }
        i = i + 1
    }
    var last: String = s.substring(s.length - 1, s.length)
    if ("?!.".indexOf(last) == (0 - 1)) {
        types = run { val _tmp = types.toMutableList(); _tmp.add("N"); _tmp }
    }
    return join(types, "|")
}

fun user_main(): Unit {
    var s: String = "hi there, how are you today? I'd like to present to you the washing machine 9001. You have been nominated to win one of these! Just make sure you don't break it"
    var result: String = sentenceType(s)
    println(result)
}

fun main() {
    user_main()
}
