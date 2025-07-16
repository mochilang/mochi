object arithmetic_integer_2 {
  def main() = {
    val a = 12345678
    val b = 98765
    println(a.toString + " + " + b.toString + " = " + a + b.toString)
    println(a.toString + " - " + b.toString + " = " + a - b.toString)
    println(a.toString + " * " + b.toString + " = " + a * b.toString)
    println(a.toString + " quo " + b.toString + " = " + (a / b).toInt.toString)
    println(a.toString + " rem " + b.toString + " = " + a % b.toString)
  }
  
  def main(args: Array[String]): Unit = {
    main()
  }
}
