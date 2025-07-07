object map_literal_dynamic {
  def main(args: Array[String]): Unit = {
    var x = 3
    var y = 4
    var m = Map("a" -> x, "b" -> y)
    print(m("a"), m("b"))
  }
}
