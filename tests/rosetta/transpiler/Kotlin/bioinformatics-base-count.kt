import java.math.BigInteger

val dna: String = ((((((((("" + "CGTAAAAAATTACAACGTCCTTTGGCTATCTCTTAAACTCCTGCTAAATG") + "CTCGTGCTTTCCAATTATGTAAGCGTTCCGAGACGGGGTGGTCGATTCTG") + "AGGACAAAGGTCAAGATGGAGCGCATCGAACGCAATAAGGATCATTTGAT") + "GGGACGTTTCGTCGACAAAGTCTTGTTTCGAGAGTAACGGCTACCGTCTT") + "CGATTCTGCTTATAACACTATGTTCTTATGAAATGGATGTTCTGAGTTGG") + "TCAGTCCCAATGTGCGGGGTTTCTTTTAGTACGTCGGGAGTGGTATTATA") + "TTTAATTTTTCTATATAGCGATCTGTATTTAAGCAATTCATTTAGGTTAT") + "CGCCGCGATGCTCGGTTCGGACCGCCAAGCATCTGGCTCCACTGCTAGTG") + "TCCTAAATTTGAATGGCAAACACAAATAAGATTTAGCAATTCGTGTAGAC") + "GACCGGGGACTTGCATGATGGGAGCAGCTTTGTTAAACTACGAACGTAAT"
val le: Int = dna.length
var i: Int = 0
var a: Int = 0
var c: Int = 0
var g: Int = 0
var t: Int = 0
var idx: Int = 0
fun padLeft(s: String, w: Int): String {
    var res: String = ""
    var n: BigInteger = (w - s.length).toBigInteger()
    while (n.compareTo(0.toBigInteger()) > 0) {
        res = res + " "
        n = n.subtract(1.toBigInteger())
    }
    return res + s
}

fun main() {
    println("SEQUENCE:")
    while (i < le) {
        var k: BigInteger = (i + 50).toBigInteger()
        if (k.compareTo(le.toBigInteger()) > 0) {
            k = le.toBigInteger()
        }
        println((padLeft(i.toString(), 5) + ": ") + dna.substring(i, (k).toInt()))
        i = i + 50
    }
    while (idx < le) {
        val ch: String = dna.substring(idx, idx + 1)
        if (ch == "A") {
            a = a + 1
        } else {
            if (ch == "C") {
                c = c + 1
            } else {
                if (ch == "G") {
                    g = g + 1
                } else {
                    if (ch == "T") {
                        t = t + 1
                    }
                }
            }
        }
        idx = idx + 1
    }
    println("")
    println("BASE COUNT:")
    println("    A: " + padLeft(a.toString(), 3))
    println("    C: " + padLeft(c.toString(), 3))
    println("    G: " + padLeft(g.toString(), 3))
    println("    T: " + padLeft(t.toString(), 3))
    println("    ------")
    println("    Σ: " + le.toString())
    println("    ======")
}
