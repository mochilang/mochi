fun is_valid(strand: String): Boolean {
    var i: Int = (0).toInt()
    while (i < strand.length) {
        var ch: String = strand.substring(i, i + 1)
        if ((((((ch != "A") && (ch != "T") as Boolean)) && (ch != "C") as Boolean)) && (ch != "G")) {
            return false
        }
        i = i + 1
    }
    return true
}

fun dna(strand: String): String {
    if (!is_valid(strand)) {
        println("ValueError: Invalid Strand")
        return ""
    }
    var result: String = ""
    var i: Int = (0).toInt()
    while (i < strand.length) {
        var ch: String = strand.substring(i, i + 1)
        if (ch == "A") {
            result = result + "T"
        } else {
            if (ch == "T") {
                result = result + "A"
            } else {
                if (ch == "C") {
                    result = result + "G"
                } else {
                    result = result + "C"
                }
            }
        }
        i = i + 1
    }
    return result
}

fun main() {
    println(dna("GCTA"))
    println(dna("ATGC"))
    println(dna("CTGA"))
    println(dna("GFGG"))
}
