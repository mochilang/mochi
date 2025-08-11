fun _sliceStr(s: String, start: Int, end: Int): String {
    val st = if (start < 0) 0 else start
    val en = if (end > s.length) s.length else end
    return if (st >= en) "" else s.substring(st, en)
}

var LETTERS_AND_SPACE: String = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz \t\n"
var LOWER: String = "abcdefghijklmnopqrstuvwxyz"
var UPPER: String = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
var ENGLISH_WORDS: MutableMap<String, Boolean> = load_dictionary()
fun to_upper(s: String): String {
    var res: String = ""
    var i: Int = (0).toInt()
    while (i < s.length) {
        var c: String = _sliceStr(s, i, i + 1)
        var j: Int = (0).toInt()
        var up: String = c
        while (j < LOWER.length) {
            if (c == _sliceStr(LOWER, j, j + 1)) {
                up = _sliceStr(UPPER, j, j + 1)
                break
            }
            j = j + 1
        }
        res = res + up
        i = i + 1
    }
    return res
}

fun char_in(chars: String, c: String): Boolean {
    var i: Int = (0).toInt()
    while (i < chars.length) {
        if (_sliceStr(chars, i, i + 1) == c) {
            return true
        }
        i = i + 1
    }
    return false
}

fun remove_non_letters(message: String): String {
    var res: String = ""
    var i: Int = (0).toInt()
    while (i < message.length) {
        var ch: String = _sliceStr(message, i, i + 1)
        if (((char_in(LETTERS_AND_SPACE, ch)) as Boolean)) {
            res = res + ch
        }
        i = i + 1
    }
    return res
}

fun split_spaces(text: String): MutableList<String> {
    var res: MutableList<String> = mutableListOf<String>()
    var current: String = ""
    var i: Int = (0).toInt()
    while (i < text.length) {
        var ch: String = _sliceStr(text, i, i + 1)
        if (ch == " ") {
            res = run { val _tmp = res.toMutableList(); _tmp.add(current); _tmp }
            current = ""
        } else {
            current = current + ch
        }
        i = i + 1
    }
    res = run { val _tmp = res.toMutableList(); _tmp.add(current); _tmp }
    return res
}

fun load_dictionary(): MutableMap<String, Boolean> {
    var words: MutableList<String> = mutableListOf("HELLO", "WORLD", "HOW", "ARE", "YOU", "THE", "QUICK", "BROWN", "FOX", "JUMPS", "OVER", "LAZY", "DOG")
    var dict: MutableMap<String, Boolean> = mutableMapOf<String, Boolean>()
    for (w in words) {
        (dict)[w] = true
    }
    return dict
}

fun get_english_count(message: String): Double {
    var upper: String = to_upper(message)
    var cleaned: String = remove_non_letters(upper)
    var possible: MutableList<String> = split_spaces(cleaned)
    var matches: Int = (0).toInt()
    var total: Int = (0).toInt()
    for (w in possible) {
        if (w != "") {
            total = total + 1
            if (w in ENGLISH_WORDS) {
                matches = matches + 1
            }
        }
    }
    if (total == 0) {
        return 0.0
    }
    return ((matches.toDouble())) / ((total.toDouble()))
}

fun is_english(message: String, word_percentage: Int, letter_percentage: Int): Boolean {
    var words_match: Boolean = (get_english_count(message) * 100.0) >= ((word_percentage.toDouble()))
    var num_letters: Int = ((remove_non_letters(message)).length).toInt()
    var letters_pct: Double = (if (message.length == 0) 0.0 else (((num_letters.toDouble())) / ((message.length.toDouble()))) * 100.0.toDouble())
    var letters_match: Boolean = letters_pct >= ((letter_percentage.toDouble()))
    return ((words_match && letters_match) as Boolean)
}

fun main() {
    println(is_english("Hello World", 20, 85).toString())
    println(is_english("llold HorWd", 20, 85).toString())
}
