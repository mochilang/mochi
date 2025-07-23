fun fields(s: String): MutableList<String> {
    var res: MutableList<String> = mutableListOf()
    var cur: String = ""
    var i: Int = 0
    while (i < s.length) {
        val c: String = s.substring(i, i + 1)
        if (c == " ") {
            if (cur.length > 0) {
                res = run { val _tmp = res.toMutableList(); _tmp.add(cur); _tmp } as MutableList<String>
                cur = ""
            }
        } else {
            cur = cur + c
        }
        i = i + 1
    }
    if (cur.length > 0) {
        res = run { val _tmp = res.toMutableList(); _tmp.add(cur); _tmp } as MutableList<String>
    }
    return res as MutableList<String>
}

fun canSpell(word: String, blks: MutableList<String>): Boolean {
    if (word.length == 0) {
        return true as Boolean
    }
    val c: String = word.substring(0, 1).toLowerCase()
    var i: Int = 0
    while (i < blks.size) {
        val b: String = blks[i]
        if ((c == b.substring(0, 1).toLowerCase()) || (c == b.substring(1, 2).toLowerCase())) {
            var rest: MutableList<String> = mutableListOf()
            var j: Int = 0
            while (j < blks.size) {
                if (j != i) {
                    rest = run { val _tmp = rest.toMutableList(); _tmp.add(blks[j]); _tmp } as MutableList<String>
                }
                j = j + 1
            }
            if (canSpell(word.substring(1, word.length), rest) as Boolean as Boolean) {
                return true as Boolean
            }
        }
        i = i + 1
    }
    return false as Boolean
}

fun newSpeller(blocks: String): (String) -> Boolean {
    val bl: MutableList<String> = fields(blocks) as MutableList<String>
    return { w: String -> canSpell(w, bl) as Boolean } as (String) -> Boolean
}

fun user_main(): Unit {
    val sp: (String) -> Boolean = newSpeller("BO XK DQ CP NA GT RE TG QD FS JW HU VI AN OB ER FS LY PC ZM") as (String) -> Boolean
    for (word in mutableListOf("A", "BARK", "BOOK", "TREAT", "COMMON", "SQUAD", "CONFUSE")) {
        println((word + " ") + sp(word).toString())
    }
}

fun main() {
    user_main()
}
