object bioinformatics_sequence_mutation {
  def randInt(s: Int, n: Int): List[Int] = {
    val next = (s * 1664525 + 1013904223) % 2147483647
    return List(next, next % n)
  }
  
  def padLeft(s: String, w: Int): String = {
    var res = ""
    var n = w - s.length
    while (n > 0) {
      res += " "
      n -= 1
    }
    return res + s
  }
  
  def makeSeq(s: Int, le: Int): List[any] = {
    val bases = "ACGT"
    var out = ""
    var i = 0
    while (i < le) {
      var r = randInt(s, 4)
      s = (r).apply(0)
      val idx = (r).apply(1).toInt
      out += bases.substring(idx, idx + 1)
      i += 1
    }
    return List(s, out)
  }
  
  def mutate(s: Int, dna: String, w: List[Int]): List[any] = {
    val bases = "ACGT"
    val le = dna.length
    var r = randInt(s, le)
    s = (r).apply(0)
    val p = (r).apply(1).toInt
    r = randInt(s, 300)
    s = (r).apply(0)
    val x = (r).apply(1).toInt
    var arr: List[String] = scala.collection.mutable.ArrayBuffer[Any]()
    var i = 0
    while (i < le) {
      arr = arr :+ dna.substring(i, i + 1)
      i += 1
    }
    if (x < (w).apply(0)) {
      r = randInt(s, 4)
      s = (r).apply(0)
      val idx = (r).apply(1).toInt
      val b = bases.substring(idx, idx + 1)
      println(((((("  Change @" + padLeft(p.toString, 3)).asInstanceOf[Int] + " '").asInstanceOf[Int] + (arr).apply(p)).asInstanceOf[Int] + "' to '").asInstanceOf[Int] + b).asInstanceOf[Int] + "'")
      arr(p) = b
    } else {
      if (x < (w).apply(0) + (w).apply(1)) {
        println(((("  Delete @" + padLeft(p.toString, 3)).asInstanceOf[Int] + " '").asInstanceOf[Int] + (arr).apply(p)).asInstanceOf[Int] + "'")
        var j = p
        while (j < arr.length - 1) {
          arr(j) = (arr).apply(j + 1)
          j += 1
        }
        arr = arr.slice(0, arr.length - 1)
      } else {
        r = randInt(s, 4)
        s = (r).apply(0)
        val idx2 = (r).apply(1).toInt
        val b = bases.substring(idx2, idx2 + 1)
        arr = arr :+ ""
        var j = arr.length - 1
        while (j > p) {
          arr(j) = (arr).apply(j - 1)
          j -= 1
        }
        println(((("  Insert @" + padLeft(p.toString, 3)).asInstanceOf[Int] + " '").asInstanceOf[Int] + b).asInstanceOf[Int] + "'")
        arr(p) = b
      }
    }
    var out = ""
    i = 0
    while (i < arr.length) {
      out += (arr).apply(i)
      i += 1
    }
    return List(s, out)
  }
  
  def prettyPrint(dna: String, rowLen: Int) = {
    println("SEQUENCE:")
    val le = dna.length
    var i = 0
    while (i < le) {
      var k = i + rowLen
      if (k > le) {
        k = le
      }
      println((padLeft(i.toString, 5) + ": ").asInstanceOf[Int] + dna.substring(i, k))
      i += rowLen
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
  
  def wstring(w: List[Int]): String = ((((("  Change: " + (w).apply(0).toString).asInstanceOf[Int] + "\n  Delete: ").asInstanceOf[Int] + (w).apply(1).toString).asInstanceOf[Int] + "\n  Insert: ").asInstanceOf[Int] + (w).apply(2).toString).asInstanceOf[Int] + "\n"
  
  def main() = {
    var seed = 1
    var res = makeSeq(seed, 250)
    seed = (res).apply(0)
    var dna = (res).apply(1).toString
    prettyPrint(dna, 50)
    val muts = 10
    val w = List(100, 100, 100)
    println("\nWEIGHTS (ex 300):")
    println(wstring(w))
    println(("MUTATIONS (" + muts.toString).asInstanceOf[Int] + "):")
    var i = 0
    while (i < muts) {
      res = mutate(seed, dna, w)
      seed = (res).apply(0)
      dna = (res).apply(1).toString
      i += 1
    }
    println("")
    prettyPrint(dna, 50)
  }
  
  def main(args: Array[String]): Unit = {
    main()
  }
}
