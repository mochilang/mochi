object map_nested_assign {
  case class Auto1(inner: Int)

  def main(args: Array[String]): Unit = {
    var data = scala.collection.mutable.Map[String, Map[String, Int]]("outer" -> (Auto1(inner = 1)))
    val _tmp0 = data("outer").updated("inner", 2)
    data = data.updated("outer", _tmp0)
    println((data("outer")("inner")))
  }
}
