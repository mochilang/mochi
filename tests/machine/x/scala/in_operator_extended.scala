object in_operator_extended {
  case class M(a: Int)

  val xs = List[Int](1, 2, 3)
  val ys = for { x <- xs; if (x % 2).asInstanceOf[Int] == 1 } yield x
  val m = Map[String, Int]("a" -> (1))
  val s = "hello"
  def main(args: Array[String]): Unit = {
    println((ys.contains(1)))
    println((ys.contains(2)))
    println((m.contains("a")))
    println((m.contains("b")))
    println((s.contains("ell")))
    println((s.contains("foo")))
  }
}
