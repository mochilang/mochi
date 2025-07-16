object cartesian_product_of_two_or_more_lists_4 {
  def listStr(xs: List[Int]): String = {
    var s = "["
    var i = 0
    while (i < xs.length) {
      s += (xs).apply(i).toString
      if (i < xs.length - 1) {
        s += " "
      }
      i += 1
    }
    s += "]"
    return s
  }
  
  def llStr(lst: List[List[Int]]): String = {
    var s = "["
    var i = 0
    while (i < lst.length) {
      s += listStr((lst).apply(i))
      if (i < lst.length - 1) {
        s += " "
      }
      i += 1
    }
    s += "]"
    return s
  }
  
  def copy(xs: List[Int]): List[Int] = {
    var out: List[Int] = scala.collection.mutable.ArrayBuffer[Any]()
    for(v <- xs) {
      out = out :+ v
    }
    return out
  }
  
  def cartN(lists: any): List[List[Int]] = {
    if ((lists).asInstanceOf[Int] == (null).asInstanceOf[Int]) {
      return List()
    }
    val a = lists.asInstanceOf[List[List[Int]]]
    if (a.length == 0) {
      return List(List())
    }
    var out: List[List[Int]] = scala.collection.mutable.ArrayBuffer[Any]()
    val last = a.length - 1
    val left = cartN(a.slice(0, last))
    for(p <- left) {
      for(x <- (a).apply(last)) {
        var row = copy(p)
        row = row :+ x
        out = out :+ row
      }
    }
    return out
  }
  
  def main() = {
    println(llStr(cartN(List(List(1, 2), List(3, 4)))))
    println(llStr(cartN(List(List(3, 4), List(1, 2)))))
    println(llStr(cartN(List(List(1, 2), List()))))
    println(llStr(cartN(List(List(), List(1, 2)))))
    println("")
    println("[")
    for(p <- cartN(List(List(1776, 1789), List(7, 12), List(4, 14, 23), List(0, 1)))) {
      println(" " + listStr(p))
    }
    println("]")
    println(llStr(cartN(List(List(1, 2, 3), List(30), List(500, 100)))))
    println(llStr(cartN(List(List(1, 2, 3), List(), List(500, 100)))))
    println("")
    println(llStr(cartN(null)))
    println(llStr(cartN(List())))
  }
  
  def main(args: Array[String]): Unit = {
    main()
  }
}
