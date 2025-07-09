object test_block {
  def main(args: Array[String]): Unit = {
    val x = 1 + 2
    assert((x).asInstanceOf[Int] == 3)
    println(("ok"))
  }
}
