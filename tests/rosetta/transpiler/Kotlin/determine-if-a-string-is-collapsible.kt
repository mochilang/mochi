fun collapse(s: String): MutableList<Any?> {
    var i: Int = 0
    var prev: String = ""
    var res: String = ""
    var orig: Int = s.length
    while (i < s.length) {
        var ch: String = s.substring(i, i + 1)
        if (ch != prev) {
            res = res + ch
            prev = ch
        }
        i = i + 1
    }
    return mutableListOf<Any?>((res as Any?), (orig as Any?), (res.length as Any?))
}

fun user_main(): Unit {
    var strings: MutableList<String> = mutableListOf("", "\"If I were two-faced, would I be wearing this one?\" --- Abraham Lincoln ", "..111111111111111111111111111111111111111111111111111111111111111777888", "I never give 'em hell, I just tell the truth, and they think it's hell. ", "                                                   ---  Harry S Truman ", "The better the 4-wheel drive, the further you'll be from help when ya get stuck!", "headmistressship", "aardvark", "😍😀🙌💃😍😍😍🙌")
    var idx: Int = 0
    while (idx < strings.size) {
        var s: String = strings[idx]!!
        var r: MutableList<Any?> = collapse(s)
        var cs: Any? = r[0] as Any?
        var olen: Any? = r[1] as Any?
        var clen: Any? = r[2] as Any?
        println(((("original : length = " + olen.toString()) + ", string = «««") + s) + "»»»")
        println(((("collapsed: length = " + clen.toString()) + ", string = «««") + (cs).toString()) + "»»»\n")
        idx = idx + 1
    }
}

fun main() {
    user_main()
}
