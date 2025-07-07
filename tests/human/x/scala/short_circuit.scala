object short_circuit {
  def boom(a: Int, b: Int): Boolean = {
    println("boom")
    true
  }
  def main(args: Array[String]): Unit = {
    println(false && boom(1,2))
    println(true || boom(1,2))
  }
}
