import java.math.BigInteger

val text: String = (((("Given$a$text$file$of$many$lines,$where$fields$within$a$line\n" + "are$delineated$by$a$single$'dollar'$character,$write$a$program\n") + "that$aligns$each$column$of$fields$by$ensuring$that$words$in$each\n") + "column$are$separated$by$at$least$one$space.\n") + "Further,$allow$for$each$word$in$a$column$to$be$either$left\n") + "justified,$right$justified,$or$center$justified$within$its$column."
val f: MutableMap<String, Any?> = newFormatter(text)
fun split(s: String, sep: String): MutableList<String> {
    var parts: MutableList<String> = mutableListOf()
    var cur: String = ""
    var i: Int = 0
    while (i < s.length) {
        if ((((sep.length > 0) && ((i + sep.length) <= s.length) as Boolean)) && (s.substring(i, i + sep.length) == sep)) {
            parts = run { val _tmp = parts.toMutableList(); _tmp.add(cur); _tmp } as MutableList<String>
            cur = ""
            i = i + sep.length
        } else {
            cur = cur + (s.substring(i, i + 1)).toString()
            i = i + 1
        }
    }
    parts = run { val _tmp = parts.toMutableList(); _tmp.add(cur); _tmp } as MutableList<String>
    return parts
}

fun rstripEmpty(words: MutableList<String>): MutableList<String> {
    var n: Int = words.size
    while ((n > 0) && (words[n - 1] == "")) {
        n = n - 1
    }
    return words.subList(0, n)
}

fun spaces(n: Int): String {
    var out: String = ""
    var i: Int = 0
    while (i < n) {
        out = out + " "
        i = i + 1
    }
    return out
}

fun pad(word: String, width: Int, align: Int): String {
    val diff: BigInteger = width - word.length
    if (align == 0) {
        return word + spaces(diff as Int)
    }
    if (align == 2) {
        return spaces(diff as Int) + word
    }
    var left: Int = diff.divide(2.toBigInteger()).toInt()
    var right: BigInteger = diff.subtract(left.toBigInteger())
    return (spaces(left) + word) + spaces(right as Int)
}

fun newFormatter(text: String): MutableMap<String, Any?> {
    var lines: MutableList<String> = split(text, "\n")
    var fmtLines: MutableList<MutableList<String>> = mutableListOf()
    var width: MutableList<Int> = mutableListOf()
    var i: Int = 0
    while (i < lines.size) {
        if (lines[i].length == 0) {
            i = i + 1
            continue
        }
        var words: MutableList<String> = rstripEmpty(split(lines[i], "$"))
        fmtLines = run { val _tmp = fmtLines.toMutableList(); _tmp.add(words); _tmp } as MutableList<MutableList<String>>
        var j: Int = 0
        while (j < words.size) {
            val wlen: Int = words[j].length
            if (j == width.size) {
                width = run { val _tmp = width.toMutableList(); _tmp.add(wlen); _tmp } as MutableList<Int>
            } else {
                if (wlen > width[j]) {
                    width[j] = wlen
                }
            }
            j = j + 1
        }
        i = i + 1
    }
    return mutableMapOf<String, Any?>("text" to (fmtLines), "width" to (width))
}

fun printFmt(f: MutableMap<String, Any?>, align: Int): Unit {
    val lines: MutableList<MutableList<String>> = (f)["text"] as Any? as MutableList<MutableList<String>>
    val width: MutableList<Int> = (f)["width"] as Any? as MutableList<Int>
    var i: Int = 0
    while (i < lines.size) {
        val words: Any? = lines[i]
        var line: String = ""
        var j: Int = 0
        while (j < words.size) {
            line = (line + pad(words[j] as String, width[j], align)) + " "
            j = j + 1
        }
        println(line)
        i = i + 1
    }
    println("")
}

fun main() {
    printFmt(f, 0)
    printFmt(f, 1)
    printFmt(f, 2)
}
