fun sortRunes(s: String): String {
    var arr: MutableList<String> = mutableListOf()
    var i: Int = 0
    while (i < s.length) {
        arr = run { val _tmp = arr.toMutableList(); _tmp.add(s.substring(i, i + 1)); _tmp } as MutableList<String>
        i = i + 1
    }
    var n: Int = arr.size
    var m: Int = 0
    while (m < n) {
        var j: Int = 0
        while (j < (n - 1)) {
            if (arr[j] > arr[j + 1]) {
                val tmp: String = arr[j]
                arr[j] = arr[j + 1]
                arr[j + 1] = tmp
            }
            j = j + 1
        }
        m = m + 1
    }
    var out: String = ""
    i = 0
    while (i < n) {
        out = out + arr[i]
        i = i + 1
    }
    return out
}

fun deranged(a: String, b: String): Boolean {
    if (a.length != b.length) {
        return false
    }
    var i: Int = 0
    while (i < a.length) {
        if (a.substring(i, i + 1) == b.substring(i, i + 1)) {
            return false
        }
        i = i + 1
    }
    return true
}

fun user_main(): Unit {
    val words: MutableList<String> = mutableListOf("constitutionalism", "misconstitutional")
    var m: MutableMap<String, MutableList<String>> = mutableMapOf<Any?, Any?>() as MutableMap<String, MutableList<String>>
    var bestLen: Int = 0
    var w1: String = ""
    var w2: String = ""
    for (w in words) {
        if (w.length <= bestLen) {
            continue
        }
        val k: String = sortRunes(w)
        if (!((k in m) as Boolean)) {
            (m)[k] = mutableListOf(w)
            continue
        }
        for (c in (m)[k] as MutableList<String>.keys) {
            if (deranged(w, c as String) as Boolean) {
                bestLen = w.length
                w1 = c as String
                w2 = w
                break
            }
        }
        (m)[k] = run { val _tmp = (m)[k] as MutableList<String>.toMutableList(); _tmp.add(w); _tmp }
    }
    println((((w1 + " ") + w2) + " : Length ") + bestLen.toString())
}

fun main() {
    user_main()
}
