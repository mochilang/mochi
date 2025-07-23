fun fields(s: String): MutableList<String> {
    var words: MutableList<String> = mutableListOf()
    var cur: String = ""
    var i: Int = 0
    while (i < s.length) {
        val ch: String = s.substring(i, i + 1)
        if ((((ch == " ") || (ch == "\n") as Boolean)) || (ch == "\t")) {
            if (cur.length > 0) {
                words = run { val _tmp = words.toMutableList(); _tmp.add(cur); _tmp } as MutableList<String>
                cur = ""
            }
        } else {
            cur = cur + ch
        }
        i = i + 1
    }
    if (cur.length > 0) {
        words = run { val _tmp = words.toMutableList(); _tmp.add(cur); _tmp } as MutableList<String>
    }
    return words as MutableList<String>
}

fun padRight(s: String, width: Int): String {
    var out: String = s
    var i: Int = s.length
    while (i < width) {
        out = out + " "
        i = i + 1
    }
    return out as String
}

fun join(xs: MutableList<String>, sep: String): String {
    var res: String = ""
    var i: Int = 0
    while (i < xs.size) {
        if (i > 0) {
            res = res + sep
        }
        res = res + xs[i]
        i = i + 1
    }
    return res as String
}

fun parseIntStr(str: String): Int {
    var i: Int = 0
    var neg: Boolean = false
    if ((str.length > 0) && (str.substring(0, 1) == "-")) {
        neg = true
        i = 1
    }
    var n: Int = 0
    val digits: MutableMap<String, Int> = mutableMapOf<String, Int>("0" to (0), "1" to (1), "2" to (2), "3" to (3), "4" to (4), "5" to (5), "6" to (6), "7" to (7), "8" to (8), "9" to (9))
    while (i < str.length) {
        n = (n * 10) + (digits)[str.substring(i, i + 1)] as Int
        i = i + 1
    }
    if (neg as Boolean) {
        n = 0 - n
    }
    return n as Int
}

fun isDigits(s: String): Boolean {
    if (s.length == 0) {
        return false as Boolean
    }
    var i: Int = 0
    while (i < s.length) {
        val ch: String = s.substring(i, i + 1)
        if ((ch < "0") || (ch > "9")) {
            return false as Boolean
        }
        i = i + 1
    }
    return true as Boolean
}

fun readTable(table: String): MutableMap<String, Any> {
    val toks: MutableList<String> = fields(table)
    var cmds: MutableList<String> = mutableListOf()
    var mins: MutableList<Int> = mutableListOf()
    var i: Int = 0
    while (i < toks.size) {
        val cmd: String = toks[i]
        var minlen: Int = cmd.length
        i = i + 1
        if ((i < toks.size) && (isDigits(toks[i]) as Boolean)) {
            val num: Int = parseIntStr(toks[i])
            if ((num >= 1) && (num < cmd.length)) {
                minlen = num
                i = i + 1
            }
        }
        cmds = run { val _tmp = cmds.toMutableList(); _tmp.add(cmd); _tmp } as MutableList<String>
        mins = run { val _tmp = mins.toMutableList(); _tmp.add(minlen); _tmp } as MutableList<Int>
    }
    return mutableMapOf<String, Any>("commands" to (cmds), "mins" to (mins)) as MutableMap<String, Any>
}

fun validate(commands: MutableList<String>, mins: MutableList<Int>, words: MutableList<String>): MutableList<String> {
    var results: MutableList<String> = mutableListOf()
    var wi: Int = 0
    while (wi < words.size) {
        val w: String = words[wi]
        var found: Boolean = false
        val wlen: Int = w.length
        var ci: Int = 0
        while (ci < commands.size) {
            val cmd: String = commands[ci]
            if ((((mins[ci] != 0) && (wlen >= mins[ci]) as Boolean)) && (wlen <= cmd.length)) {
                val c: String = cmd.toUpperCase()
                val ww: String = w.toUpperCase()
                if (c.substring(0, wlen) == ww) {
                    results = run { val _tmp = results.toMutableList(); _tmp.add(c); _tmp } as MutableList<String>
                    found = true
                    break
                }
            }
            ci = ci + 1
        }
        if (!found) {
            results = run { val _tmp = results.toMutableList(); _tmp.add("*error*"); _tmp } as MutableList<String>
        }
        wi = wi + 1
    }
    return results as MutableList<String>
}

fun user_main(): Unit {
    val table: String = ((((((("" + "add 1  alter 3  backup 2  bottom 1  Cappend 2  change 1  Schange  Cinsert 2  Clast 3 ") + "compress 4 copy 2 count 3 Coverlay 3 cursor 3  delete 3 Cdelete 2  down 1  duplicate ") + "3 xEdit 1 expand 3 extract 3  find 1 Nfind 2 Nfindup 6 NfUP 3 Cfind 2 findUP 3 fUP 2 ") + "forward 2  get  help 1 hexType 4  input 1 powerInput 3  join 1 split 2 spltJOIN load ") + "locate 1 Clocate 2 lowerCase 3 upperCase 3 Lprefix 2  macro  merge 2 modify 3 move 2 ") + "msg  next 1 overlay 1 parse preserve 4 purge 3 put putD query 1 quit  read recover 3 ") + "refresh renum 3 repeat 3 replace 1 Creplace 2 reset 3 restore 4 rgtLEFT right 2 left ") + "2  save  set  shift 2  si  sort  sos  stack 3 status 4 top  transfer 3  type 1  up 1 "
    val sentence: String = "riG   rePEAT copies  put mo   rest    types   fup.    6\npoweRin"
    val tbl: MutableMap<String, Any> = readTable(table)
    val commands: MutableList<String> = (tbl)["commands"]!! as MutableList<String>
    val mins: MutableList<Int> = (tbl)["mins"]!! as MutableList<Int>
    val words: MutableList<String> = fields(sentence)
    val results: MutableList<String> = validate(commands, mins, words)
    var out1: String = "user words:"
    var k: Int = 0
    while (k < words.size) {
        out1 = out1 + " "
        if (k < (words.size - 1)) {
            out1 = out1 + (padRight(words[k], results[k].length)).toString()
        } else {
            out1 = out1 + words[k]
        }
        k = k + 1
    }
    println(out1)
    println("full words: " + (join(results, " ")).toString())
}

fun main() {
    user_main()
}
