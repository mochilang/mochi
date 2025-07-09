object map_nested_assign {
  def main(args: Array[String]): Unit = {
    var data = scala.collection.mutable.Map("outer" -> (Map("inner" -> (1))))
    data("outer")("inner") = 2
    println((data("outer")("inner")))
  }
}
