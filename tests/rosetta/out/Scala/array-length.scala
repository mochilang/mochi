object array_length {
  def main() = {
    val arr = List("apple", "orange", "pear")
    println("Length of " + arr.toString + " is " + arr.length.toString + ".")
  }
  
  def main(args: Array[String]): Unit = {
    main()
  }
}
