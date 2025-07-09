object in_operator_extended {
  def main(args: Array[String]): Unit = {
    val xs = List(1, 2, 3)
    val ys = for { x <- xs; if (x % 2).asInstanceOf[Int] == 1 } yield x
    println((ys.contains(1)))
    println((ys.contains(2)))
    val m = Map("a" -> (1))
    println((m.contains("a")))
    println((m.contains("b")))
    val s = "hello"
    println((s.contains("ell")))
    println((s.contains("foo")))
  }
}
