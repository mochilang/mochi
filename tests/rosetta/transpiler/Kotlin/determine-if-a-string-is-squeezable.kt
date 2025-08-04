var strings: MutableList<String> = mutableListOf("", "\"If I were two-faced, would I be wearing this one?\" --- Abraham Lincoln ", "..1111111111111111111111111111111111111111111111111111111111111117777888", "I never give 'em hell, I just tell the truth, and they think it's hell. ", "                                                   ---  Harry S Truman  ", "The better the 4-wheel drive, the further you'll be from help when ya get stuck!", "headmistressship", "aardvark", "😍😀🙌💃😍😍😍🙌")
var chars: MutableList<MutableList<String>> = mutableListOf(mutableListOf(" "), mutableListOf("-"), mutableListOf("7"), mutableListOf("."), mutableListOf(" ", "-", "r"), mutableListOf("e"), mutableListOf("s"), mutableListOf("a"), mutableListOf("😍"))
var i: Int = 0
fun padLeft(n: Int, width: Int): String {
    var s: String = n.toString()
    while (s.length < width) {
        s = " " + s
    }
    return s
}

fun squeeze(s: String, ch: String): String {
    var out: String = ""
    var prev: Boolean = false
    var i: Int = 0
    while (i < s.length) {
        var c: String = s.substring(i, i + 1)
        if (c == ch) {
            if (!prev) {
                out = out + c
                prev = true
            }
        } else {
            out = out + c
            prev = false
        }
        i = i + 1
    }
    return out
}

fun main() {
    while (i < strings.size) {
        var j: Int = 0
        var s: String = strings[i]!!
        while (j < (chars[i]!!).size) {
            var c: String = (((chars[i]!!) as MutableList<String>))[j]!!
            var ss: String = squeeze(s, c)
            println(("specified character = '" + c) + "'")
            println(((("original : length = " + padLeft(s.length, 2)) + ", string = «««") + s) + "»»»")
            println(((("squeezed : length = " + padLeft(ss.length, 2)) + ", string = «««") + ss) + "»»»")
            println("")
            j = j + 1
        }
        i = i + 1
    }
}
