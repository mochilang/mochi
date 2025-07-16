object boolean_values {
  def parseBool(s: String): Boolean = {
    val l = lower(s)
    if (l == "1" || l == "t" || l == true || l == "yes" || l == "y") {
      return true
    }
    return false
  }
  
  def main() = {
    var n = true
    println(n)
    println("bool")
    n = !n
    println(n)
    val x = 5
    val y = 8
    println(s"x == y: ${x == y}")
    println(s"x < y: ${x < y}")
    println("\nConvert String into Boolean Data type\n")
    val str1 = "japan"
    println(s"Before: string")
    val bolStr = parseBool(str1)
    println(s"After: bool")
  }
  
  def main(args: Array[String]): Unit = {
    main()
  }
}
