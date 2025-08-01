// Generated by Mochi compiler v0.10.26 on 2025-07-16T09:55:24Z
func padLeft(_ s: String, _ w: Int) -> String {
    var res = ""
    var n = w - s.count
    while n > 0 {
        res = res + " "
        n = n - 1
    }
    return res + s
}
let dna = "" + "CGTAAAAAATTACAACGTCCTTTGGCTATCTCTTAAACTCCTGCTAAATG" + "CTCGTGCTTTCCAATTATGTAAGCGTTCCGAGACGGGGTGGTCGATTCTG" + "AGGACAAAGGTCAAGATGGAGCGCATCGAACGCAATAAGGATCATTTGAT" + "GGGACGTTTCGTCGACAAAGTCTTGTTTCGAGAGTAACGGCTACCGTCTT" + "CGATTCTGCTTATAACACTATGTTCTTATGAAATGGATGTTCTGAGTTGG" + "TCAGTCCCAATGTGCGGGGTTTCTTTTAGTACGTCGGGAGTGGTATTATA" + "TTTAATTTTTCTATATAGCGATCTGTATTTAAGCAATTCATTTAGGTTAT" + "CGCCGCGATGCTCGGTTCGGACCGCCAAGCATCTGGCTCCACTGCTAGTG" + "TCCTAAATTTGAATGGCAAACACAAATAAGATTTAGCAATTCGTGTAGAC" + "GACCGGGGACTTGCATGATGGGAGCAGCTTTGTTAAACTACGAACGTAAT"
print("SEQUENCE:")
let le = dna.count
var i = 0
while i < le {
    var k = i + 50
    if k > le {
        k = le
    }
    print(padLeft(String(i), 5) + ": " + Array(dna[i..<k]))
    i = i + 50
}
var a = 0
var c = 0
var g = 0
var t = 0
var idx = 0
while idx < le {
    let ch = String(dna[dna.index(dna.startIndex, offsetBy: idx)..<dna.index(dna.startIndex, offsetBy: idx + 1)])
    if ch == "A" {
        a = a + 1
    }
    else {
        if ch == "C" {
            c = c + 1
        }
        else {
            if ch == "G" {
                g = g + 1
            }
            else {
                if ch == "T" {
                    t = t + 1
                }
            }
        }
    }
    idx = idx + 1
}
print("")
print("BASE COUNT:")
print("    A: " + padLeft(String(a), 3))
print("    C: " + padLeft(String(c), 3))
print("    G: " + padLeft(String(g), 3))
print("    T: " + padLeft(String(t), 3))
print("    ------")
print("    Σ: " + String(le))
print("    ======")
