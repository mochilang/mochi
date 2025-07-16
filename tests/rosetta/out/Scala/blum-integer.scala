object blum_integer {
  def isPrime(n: Int): Boolean = {
    if (n < 2) {
      return false
    }
    if (n % 2 == 0) {
      return n == 2
    }
    if (n % 3 == 0) {
      return n == 3
    }
    var d = 5
    while (d * d <= n) {
      if (n % d == 0) {
        return false
      }
      d += 2
      if (n % d == 0) {
        return false
      }
      d += 4
    }
    return true
  }
  
  def firstPrimeFactor(n: Int): Int = {
    if (n == 1) {
      return 1
    }
    if (n % 3 == 0) {
      return 3
    }
    if (n % 5 == 0) {
      return 5
    }
    var inc = scala.collection.mutable.ArrayBuffer(4, 2, 4, 2, 4, 6, 2, 6)
    var k = 7
    var i = 0
    while (k * k <= n) {
      if (n % k == 0) {
        return k
      }
      k += (inc).apply(i)
      i = (i + 1) % inc.length
    }
    return n
  }
  
  def indexOf(s: String, ch: String): Int = {
    var i = 0
    while (i < s.length) {
      if (s.substring(i, i + 1) == ch) {
        return i
      }
      i += 1
    }
    return -1
  }
  
  def padLeft(n: Int, width: Int): String = {
    var s = n.toString
    while (s.length < width) {
      s = " " + s
    }
    return s
  }
  
  def formatFloat(f: Double, prec: Int): String = {
    val s = f.toString
    val idx = indexOf(s, ".")
    if (idx < 0) {
      return s
    }
    val need = idx + 1 + prec
    if (s.length > need) {
      return s.substring(0, need)
    }
    return s
  }
  
  def main() = {
    var blum: List[Int] = scala.collection.mutable.ArrayBuffer[Any]()
    var counts = scala.collection.mutable.ArrayBuffer(0, 0, 0, 0)
    var digits = scala.collection.mutable.ArrayBuffer(1, 3, 7, 9)
    var i = 1
    var bc = 0
    while (true) {
      val p = firstPrimeFactor(i)
      if (p % 4 == 3) {
        val q = (i / p).toInt
        if ((q != p && q % 4).asInstanceOf[Int] == 3 && isPrime(q)) {
          if (bc < 50) {
            blum = blum :+ i
          }
          val d = i % 10
          if (d == 1) {
            counts(0) = (counts).apply(0) + 1
          } else {
            if (d == 3) {
              counts(1) = (counts).apply(1) + 1
            } else {
              if (d == 7) {
                counts(2) = (counts).apply(2) + 1
              } else {
                if (d == 9) {
                  counts(3) = (counts).apply(3) + 1
                }
              }
            }
          }
          bc += 1
          if (bc == 50) {
            println("First 50 Blum integers:")
            var idx = 0
            while (idx < 50) {
              var line = ""
              var j = 0
              while (j < 10) {
                line = (line + padLeft((blum).apply(idx), 3)).asInstanceOf[Int] + " "
                idx += 1
                j += 1
              }
              println(line.substring(0, line.length - 1))
            }
            return
          }
        }
      }
      if (i % 5 == 3) {
        i += 4
      } else {
        i += 2
      }
    }
  }
  
  def main(args: Array[String]): Unit = {
    main()
  }
}
