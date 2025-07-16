object cartesian_product_of_two_or_more_lists_1 {
  def cart2(a: List[Int], b: List[Int]): List[List[Int]] = {
    var p: List[List[Int]] = scala.collection.mutable.ArrayBuffer[Any]()
    for(x <- a) {
      for(y <- b) {
        p = p :+ List(x, y)
      }
    }
    return p
  }
  
  def llStr(lst: List[List[Int]]): String = {
    var s = "["
    var i = 0
    while (i < lst.length) {
      var row = (lst).apply(i)
      s += "["
      var j = 0
      while (j < row.length) {
        s += (row).apply(j).toString
        if (j < row.length - 1) {
          s += " "
        }
        j += 1
      }
      s += "]"
      if (i < lst.length - 1) {
        s += " "
      }
      i += 1
    }
    s += "]"
    return s
  }
  
  def main() = {
    println(llStr(cart2(List(1, 2), List(3, 4))))
    println(llStr(cart2(List(3, 4), List(1, 2))))
    println(llStr(cart2(List(1, 2), List())))
    println(llStr(cart2(List(), List(1, 2))))
  }
  
  def main(args: Array[String]): Unit = {
    main()
  }
}
