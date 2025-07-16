object arrays {
  def listStr(xs: List[Int]): String = {
    var s = "["
    var i = 0
    while (i < xs.length) {
      s += (xs).apply(i).toString
      if (i + 1 < xs.length) {
        s += " "
      }
      i += 1
    }
    s += "]"
    return s
  }
  
  def main(args: Array[String]): Unit = {
    var a = scala.collection.mutable.ArrayBuffer(0, 0, 0, 0, 0)
    println("len(a) = " + a.length.toString)
    println("a = " + listStr(a))
    a(0) = 3
    println("a = " + listStr(a))
    println("a[0] = " + (a).apply(0).toString)
    var s = a.slice(0, 4)
    var cap_s = 5
    println("s = " + listStr(s))
    println("len(s) = " + s.length.toString + "  cap(s) = " + cap_s.toString)
    s = a.slice(0, 5)
    println("s = " + listStr(s))
    a(0) = 22
    s(0) = 22
    println("a = " + listStr(a))
    println("s = " + listStr(s))
    s = s :+ 4
    s = s :+ 5
    s = s :+ 6
    cap_s = 10
    println("s = " + listStr(s))
    println("len(s) = " + s.length.toString + "  cap(s) = " + cap_s.toString)
    a(4) = -1
    println("a = " + listStr(a))
    println("s = " + listStr(s))
    s = List()
    for(i <- 0 until 8) {
      s = s :+ 0
    }
    cap_s = 8
    println("s = " + listStr(s))
    println("len(s) = " + s.length.toString + "  cap(s) = " + cap_s.toString)
  }
}
