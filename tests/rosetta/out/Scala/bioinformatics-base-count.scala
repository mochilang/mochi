object bioinformatics_base_count {
  def padLeft(s: String, w: Int): String = {
    var res = ""
    var n = w - s.length
    while (n > 0) {
      res += " "
      n -= 1
    }
    return res + s
  }
  
  def main(args: Array[String]): Unit = {
    val dna = "" + "CGTAAAAAATTACAACGTCCTTTGGCTATCTCTTAAACTCCTGCTAAATG" + "CTCGTGCTTTCCAATTATGTAAGCGTTCCGAGACGGGGTGGTCGATTCTG" + "AGGACAAAGGTCAAGATGGAGCGCATCGAACGCAATAAGGATCATTTGAT" + "GGGACGTTTCGTCGACAAAGTCTTGTTTCGAGAGTAACGGCTACCGTCTT" + "CGATTCTGCTTATAACACTATGTTCTTATGAAATGGATGTTCTGAGTTGG" + "TCAGTCCCAATGTGCGGGGTTTCTTTTAGTACGTCGGGAGTGGTATTATA" + "TTTAATTTTTCTATATAGCGATCTGTATTTAAGCAATTCATTTAGGTTAT" + "CGCCGCGATGCTCGGTTCGGACCGCCAAGCATCTGGCTCCACTGCTAGTG" + "TCCTAAATTTGAATGGCAAACACAAATAAGATTTAGCAATTCGTGTAGAC" + "GACCGGGGACTTGCATGATGGGAGCAGCTTTGTTAAACTACGAACGTAAT"
    println("SEQUENCE:")
    val le = dna.length
    var i = 0
    while (i < le) {
      var k = i + 50
      if (k > le) {
        k = le
      }
      println(padLeft(i.toString, 5) + ": " + dna.substring(i, k))
      i += 50
    }
    var a = 0
    var c = 0
    var g = 0
    var t = 0
    var idx = 0
    while (idx < le) {
      val ch = dna.substring(idx, idx + 1)
      if (ch == "A") {
        a += 1
      } else {
        if (ch == "C") {
          c += 1
        } else {
          if (ch == "G") {
            g += 1
          } else {
            if (ch == "T") {
              t += 1
            }
          }
        }
      }
      idx += 1
    }
    println("")
    println("BASE COUNT:")
    println("    A: " + padLeft(a.toString, 3))
    println("    C: " + padLeft(c.toString, 3))
    println("    G: " + padLeft(g.toString, 3))
    println("    T: " + padLeft(t.toString, 3))
    println("    ------")
    println("    Σ: " + le.toString)
    println("    ======")
  }
}
