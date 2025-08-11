var LETTERS: String = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
var LOWERCASE: String = "abcdefghijklmnopqrstuvwxyz"
fun char_to_lower(c: String): String {
    var i: Int = (0).toInt()
    while (i < LETTERS.length) {
        if (c == LETTERS.substring(i, i + 1)) {
            return LOWERCASE.substring(i, i + 1)
        }
        i = i + 1
    }
    return c
}

fun normalize(input_str: String): String {
    var res: String = ""
    var i: Int = (0).toInt()
    while (i < input_str.length) {
        var ch: String = input_str.substring(i, i + 1)
        var lc: String = char_to_lower(ch)
        if ((lc >= "a") && (lc <= "z")) {
            res = res + lc
        }
        i = i + 1
    }
    return res
}

fun can_string_be_rearranged_as_palindrome_counter(input_str: String): Boolean {
    var s: String = normalize(input_str)
    var freq: MutableMap<String, Int> = mutableMapOf<String, Int>()
    var i: Int = (0).toInt()
    while (i < s.length) {
        var ch: String = s.substring(i, i + 1)
        if (ch in freq) {
            (freq)[ch] = (freq)[ch] as Int + 1
        } else {
            (freq)[ch] = 1
        }
        i = i + 1
    }
    var odd: Int = (0).toInt()
    for (key in freq.keys) {
        if ((Math.floorMod((freq)[key] as Int, 2)) != 0) {
            odd = odd + 1
        }
    }
    return odd < 2
}

fun can_string_be_rearranged_as_palindrome(input_str: String): Boolean {
    var s: String = normalize(input_str)
    if (s.length == 0) {
        return true
    }
    var character_freq_dict: MutableMap<String, Int> = mutableMapOf<String, Int>()
    var i: Int = (0).toInt()
    while (i < s.length) {
        var character: String = s.substring(i, i + 1)
        if (character in character_freq_dict) {
            (character_freq_dict)[character] = (character_freq_dict)[character] as Int + 1
        } else {
            (character_freq_dict)[character] = 1
        }
        i = i + 1
    }
    var odd_char: Int = (0).toInt()
    for (character_key in character_freq_dict.keys) {
        var character_count: Int = ((character_freq_dict)[character_key] as Int).toInt()
        if ((Math.floorMod(character_count, 2)) != 0) {
            odd_char = odd_char + 1
        }
    }
    return (!(odd_char > 1) as Boolean)
}

fun main() {
    println(can_string_be_rearranged_as_palindrome_counter("Momo"))
    println(can_string_be_rearranged_as_palindrome_counter("Mother"))
    println(can_string_be_rearranged_as_palindrome("Momo"))
    println(can_string_be_rearranged_as_palindrome("Mother"))
}
