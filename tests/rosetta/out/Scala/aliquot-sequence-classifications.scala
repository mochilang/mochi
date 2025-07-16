object aliquot_sequence_classifications {
  val THRESHOLD = 140737488355328
  def indexOf(xs: List[Int], value: Int): Int = {
    var i = 0
    while (i < xs.length) {
      if ((xs).apply(i) == value) {
        return i
      }
      i += 1
    }
    return 0 - 1
  }
  
  def contains(xs: List[Int], value: Int): Boolean = indexOf(xs, value) != 0 - 1
  
  def maxOf(a: Int, b: Int): Int = {
    if (a > b) {
      return a
    } else {
      return b
    }
  }
  
  def intSqrt(n: Int): Int = {
    if (n == 0) {
      return 0
    }
    var x = n
    var y = (x + 1) / 2
    while (y < x) {
      x = y
      y = (x + n / x) / 2
    }
    return x
  }
  
  def sumProperDivisors(n: Int): Int = {
    if (n < 2) {
      return 0
    }
    val sqrt = intSqrt(n)
    var sum = 1
    var i = 2
    while (i <= sqrt) {
      if (n % i == 0) {
        sum = sum + i + n / i
      }
      i += 1
    }
    if (sqrt * sqrt == n) {
      sum -= sqrt
    }
    return sum
  }
  
  def classifySequence(k: Int): Map[String, any] = {
    var last = k
    var seq: List[Int] = scala.collection.mutable.ArrayBuffer(k)
    while (true) {
      last = sumProperDivisors(last)
      seq = seq :+ last
      val n = seq.length
      var aliquot = ""
      if (last == 0) {
        aliquot = "Terminating"
      } else {
        if (n == 2 && last == k) {
          aliquot = "Perfect"
        } else {
          if (n == 3 && last == k) {
            aliquot = "Amicable"
          } else {
            if (n >= 4 && last == k) {
              aliquot = "Sociable[" + n - 1.toString + "]"
            } else {
              if (last == (seq).apply(n - 2)) {
                aliquot = "Aspiring"
              } else {
                if (contains(seq.slice(1, maxOf(1, n - 2)), last)) {
                  val idx = indexOf(seq, last)
                  aliquot = "Cyclic[" + n - 1 - idx.toString + "]"
                } else {
                  if (n == 16 || last > THRESHOLD) {
                    aliquot = "Non-Terminating"
                  }
                }
              }
            }
          }
        }
      }
      if (aliquot != "") {
        return Map("seq" -> seq, "aliquot" -> aliquot)
      }
    }
    return Map("seq" -> seq, "aliquot" -> "")
  }
  
  def padLeft(n: Int, w: Int): String = {
    var s = n.toString
    while (s.length < w) {
      s = " " + s
    }
    return s
  }
  
  def padRight(s: String, w: Int): String = {
    var r = s
    while (r.length < w) {
      r += " "
    }
    return r
  }
  
  def joinWithCommas(seq: List[Int]): String = {
    var s = "["
    var i = 0
    while (i < seq.length) {
      s += (seq).apply(i).toString
      if (i < seq.length - 1) {
        s += ", "
      }
      i += 1
    }
    s += "]"
    return s
  }
  
  def main() = {
    println("Aliquot classifications - periods for Sociable/Cyclic in square brackets:\n")
    var k = 1
    while (k <= 10) {
      val res = classifySequence(k)
      println(padLeft(k, 2) + ": " + padRight((res).apply("aliquot").toString, 15) + " " + joinWithCommas((res).apply("seq").asInstanceOf[List[Int]]))
      k += 1
    }
    println("")
    val s = List(11, 12, 28, 496, 220, 1184, 12496, 1264460, 790, 909, 562, 1064, 1488)
    var i = 0
    while (i < s.length) {
      val val = (s).apply(i)
      val res = classifySequence(val)
      println(padLeft(val, 7) + ": " + padRight((res).apply("aliquot").toString, 15) + " " + joinWithCommas((res).apply("seq").asInstanceOf[List[Int]]))
      i += 1
    }
    println("")
    val big = 15355717786080
    val r = classifySequence(big)
    println(big.toString + ": " + padRight((r).apply("aliquot").toString, 15) + " " + joinWithCommas((r).apply("seq").asInstanceOf[List[Int]]))
  }
  
  def main(args: Array[String]): Unit = {
    main()
  }
}
