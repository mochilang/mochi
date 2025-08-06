var CHARS: MutableList<String> = mutableListOf("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O", "P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y", "Z", "1", "2", "3", "4", "5", "6", "7", "8", "9", "0", "&", "@", ":", ",", ".", "'", "\"", "?", "/", "=", "+", "-", "(", ")", "!", " ")
var CODES: MutableList<String> = mutableListOf(".-", "-...", "-.-.", "-..", ".", "..-.", "--.", "....", "..", ".---", "-.-", ".-..", "--", "-.", "---", ".--.", "--.-", ".-.", "...", "-", "..-", "...-", ".--", "-..-", "-.--", "--..", ".----", "..---", "...--", "....-", ".....", "-....", "--...", "---..", "----.", "-----", ".-...", ".--.-.", "---...", "--..--", ".-.-.-", ".----.", ".-..-.", "..--..", "-..-.", "-...-", ".-.-.", "-....-", "-.--.", "-.--.-", "-.-.--", "/")
var msg: String = "Morse code here!"
fun to_upper_char(c: String): String {
    if (c == "a") {
        return "A"
    }
    if (c == "b") {
        return "B"
    }
    if (c == "c") {
        return "C"
    }
    if (c == "d") {
        return "D"
    }
    if (c == "e") {
        return "E"
    }
    if (c == "f") {
        return "F"
    }
    if (c == "g") {
        return "G"
    }
    if (c == "h") {
        return "H"
    }
    if (c == "i") {
        return "I"
    }
    if (c == "j") {
        return "J"
    }
    if (c == "k") {
        return "K"
    }
    if (c == "l") {
        return "L"
    }
    if (c == "m") {
        return "M"
    }
    if (c == "n") {
        return "N"
    }
    if (c == "o") {
        return "O"
    }
    if (c == "p") {
        return "P"
    }
    if (c == "q") {
        return "Q"
    }
    if (c == "r") {
        return "R"
    }
    if (c == "s") {
        return "S"
    }
    if (c == "t") {
        return "T"
    }
    if (c == "u") {
        return "U"
    }
    if (c == "v") {
        return "V"
    }
    if (c == "w") {
        return "W"
    }
    if (c == "x") {
        return "X"
    }
    if (c == "y") {
        return "Y"
    }
    if (c == "z") {
        return "Z"
    }
    return c
}

fun to_upper(s: String): String {
    var res: String = ""
    var i: Int = 0
    while (i < s.length) {
        res = res + to_upper_char(s[i].toString())
        i = i + 1
    }
    return res
}

fun index_of(xs: MutableList<String>, target: String): Int {
    var i: Int = 0
    while (i < xs.size) {
        if (xs[i]!! == target) {
            return i
        }
        i = i + 1
    }
    return 0 - 1
}

fun encrypt(message: String): String {
    var msg: String = to_upper(message)
    var res: String = ""
    var i: Int = 0
    while (i < msg.length) {
        var c: String = msg[i].toString()
        var idx: Int = index_of(CHARS, c)
        if (idx >= 0) {
            if (res != "") {
                res = res + " "
            }
            res = res + CODES[idx]!!
        }
        i = i + 1
    }
    return res
}

fun split_spaces(s: String): MutableList<String> {
    var res: MutableList<String> = mutableListOf<String>()
    var current: String = ""
    var i: Int = 0
    while (i < s.length) {
        var ch: String = s[i].toString()
        if (ch == " ") {
            if (current != "") {
                res = run { val _tmp = res.toMutableList(); _tmp.add(current); _tmp }
                current = ""
            }
        } else {
            current = current + ch
        }
        i = i + 1
    }
    if (current != "") {
        res = run { val _tmp = res.toMutableList(); _tmp.add(current); _tmp }
    }
    return res
}

fun decrypt(message: String): String {
    var parts: MutableList<String> = split_spaces(message)
    var res: String = ""
    for (code in parts) {
        var idx: Int = index_of(CODES, code)
        if (idx >= 0) {
            res = res + CHARS[idx]!!
        }
    }
    return res
}

fun main() {
    println(msg)
    var enc: String = encrypt(msg)
    println(enc)
    var dec: String = decrypt(enc)
    println(dec)
    println(encrypt("Sos!"))
    println(decrypt("... --- ... -.-.--"))
}
