object binary_strings {
  def char(n: Int): String = {
    val letters = "abcdefghijklmnopqrstuvwxyz"
    val idx = n - 97
    if (idx < 0 || idx >= letters.length) {
      return "?"
    }
    return letters.substring(idx, idx + 1)
  }
  
  def fromBytes(bs: List[Int]): String = {
    var s = ""
    var i = 0
    while (i < bs.length) {
      s += char((bs).apply(i))
      i += 1
    }
    return s
  }
  
  def main(args: Array[String]): Unit = {
    var b: List[Int] = scala.collection.mutable.ArrayBuffer(98, 105, 110, 97, 114, 121)
    println(b.toString)
    var c: List[Int] = b
    println(c.toString)
    println(b == c.toString)
    var d: List[Int] = scala.collection.mutable.ArrayBuffer[Any]()
    var i = 0
    while (i < b.length) {
      d = d :+ (b).apply(i)
      i += 1
    }
    d(1) = 97
    d(4) = 110
    println(fromBytes(b))
    println(fromBytes(d))
    println(b.length == 0.toString)
    var z = b :+ 122
    println(fromBytes(z))
    var sub = b.slice(1, 3)
    println(fromBytes(sub))
    var f: List[Int] = scala.collection.mutable.ArrayBuffer[Any]()
    i = 0
    while (i < d.length) {
      val val = (d).apply(i)
      if (val == 110) {
        f = f :+ 109
      } else {
        f = f :+ val
      }
      i += 1
    }
    println(fromBytes(d) + " -> " + fromBytes(f))
    var rem: List[Int] = scala.collection.mutable.ArrayBuffer[Any]()
    rem = rem :+ (b).apply(0)
    i = 3
    while (i < b.length) {
      rem = rem :+ (b).apply(i)
      i += 1
    }
    println(fromBytes(rem))
  }
}
