object assertions {
  def main() = {
    val x = 43
    if (x != 42) {
      println("Assertion failed")
    } else {
      println("Assertion passed")
    }
  }
  
  def main(args: Array[String]): Unit = {
    main()
  }
}
