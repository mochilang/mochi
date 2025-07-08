object bool_chain {
  def boom(): Boolean = {
    println("boom")
    return true
  }
  
  def main(args: Array[String]): Unit = {
    println((1 < 2) && (2 < 3) && (3 < 4))
    println((1 < 2) && (2 > 3) && boom())
    println((1 < 2) && (2 < 3) && (3 > 4) && boom())
  }
}
