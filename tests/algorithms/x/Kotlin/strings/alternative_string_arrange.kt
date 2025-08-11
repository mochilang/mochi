fun alternative_string_arrange(first_str: String, second_str: String): String {
    var len1: Int = (first_str.length).toInt()
    var len2: Int = (second_str.length).toInt()
    var res: String = ""
    var i: Int = (0).toInt()
    while ((i < len1) || (i < len2)) {
        if (i < len1) {
            res = res + first_str[i].toString()
        }
        if (i < len2) {
            res = res + second_str[i].toString()
        }
        i = i + 1
    }
    return res
}

fun main() {
    println(alternative_string_arrange("ABCD", "XY"))
    println(alternative_string_arrange("XY", "ABCD"))
    println(alternative_string_arrange("AB", "XYZ"))
    println(alternative_string_arrange("ABC", ""))
}
