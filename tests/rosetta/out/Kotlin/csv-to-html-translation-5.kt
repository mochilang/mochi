// Generated by Mochi compiler v0.10.28 on 2006-01-02T15:04:05Z
fun <T> append(list: MutableList<T>, item: T): MutableList<T> {
    val res = list.toMutableList()
    res.add(item)
    return res
}
// Code generated from csv-to-html-translation-5.mochi

val c = "Character,Speech\n" + "The multitude,The messiah! Show us the messiah!\n" + "Brians mother,<angry>Now you listen here! He's not the messiah; he's a very naughty boy! Now go away!</angry>\n" + "The multitude,Who are you?\n" + "Brians mother,I'm his mother; that's who!\n" + "The multitude,Behold his mother! Behold his mother!"

var rows: MutableList<MutableList<String>> = mutableListOf<MutableList<String>>()

/**
 * Auto-generated from Mochi
 * @param s String
 * @param sep String
 * @return MutableList<String>
 */
fun split(s: String, sep: String): MutableList<String> {
    var out: MutableList<String> = mutableListOf<String>()
    var start = 0
    var i = 0
    val n = sep.length
    while (i <= s.length - n) {
        if (s.substring(i, i + n) == sep) {
            out = append(out, s.substring(start, i))
            i = i + n
            start = i
        }
        else {
            i = i + 1
        }
    }
    out = append(out, s.substring(start, s.length))
    return out
}

/**
 * Auto-generated from Mochi
 * @param s String
 * @return String
 */
fun htmlEscape(s: String): String {
    var out = ""
    var i = 0
    while (i < s.length) {
        val ch = s.substring(i, i + 1)
        if (ch == "&") {
            out = out + "&amp;"
        }
        else
        if (ch == "<") {
            out = out + "&lt;"
        }
        else
        if (ch == ">") {
            out = out + "&gt;"
        }
        else {
            out = out + ch
        }
        i = i + 1
    }
    return out
}

fun main() {
    for (line in split(c, "\n")) {
        rows = append(rows, split(line, ","))
    }
    println("<table>")
    for (row in rows) {
        var cells = ""
        for (cell in row) {
            cells = cells + "<td>" + htmlEscape(cell) + "</td>"
        }
        println("    <tr>" + cells + "</tr>")
    }
    println("</table>")
}
