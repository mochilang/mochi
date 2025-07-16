object cartesian_product_of_two_or_more_lists_2 {
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
  
  def cartN(lists: any): List[List[Int]] = {
    if ((lists).asInstanceOf[Int] == (null).asInstanceOf[Int]) {
      return List()
    }
    val a = lists.asInstanceOf[List[List[Int]]]
    if (a.length == 0) {
      return List(List())
    }
    var c = 1
    for(xs <- a) {
      c *= xs.length
    }
    if (c == 0) {
      return List()
    }
    var res: List[List[Int]] = scala.collection.mutable.ArrayBuffer[Any]()
    var idx: List[Int] = scala.collection.mutable.ArrayBuffer[Any]()
    for(_ <- a) {
      idx = idx :+ 0
    }
    var n = a.length
    var count = 0
    while (count < c) {
      var row: List[Int] = scala.collection.mutable.ArrayBuffer[Any]()
      var j = 0
      while (j < n) {
        row = row :+ ((a).apply(j)).apply((idx).apply(j))
        j += 1
      }
      res = res :+ row
      var k = n - 1
      while (k >= 0) {
        idx(k) = (idx).apply(k) + 1
        if ((idx).apply(k) < (a).apply(k).length) {
          return
        }
        idx(k) = 0
        k -= 1
      }
      count += 1
    }
    return res
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
