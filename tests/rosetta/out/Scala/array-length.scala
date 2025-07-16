object array_length {
  def main() = {
    val arr = List("apple", "orange", "pear")
    println(((("Length of " + arr.toString).asInstanceOf[Int] + " is ").asInstanceOf[Int] + arr.length.toString).asInstanceOf[Int] + ".")
  }
  
  def main(args: Array[String]): Unit = {
    main()
  }
}
