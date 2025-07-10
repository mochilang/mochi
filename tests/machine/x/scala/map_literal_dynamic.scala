object map_literal_dynamic {
  def main(args: Array[String]): Unit = {
    var x = 3
    var y = 4
    var m = scala.collection.mutable.Map[String, Int]("a" -> (x), "b" -> (y))
    println((m("a")) + " " + (m("b")))
  }
}
