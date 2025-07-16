object arithmetic_integer_2 {
  def main() = {
    val a = 12345678
    val b = 98765
    println((((a.toString + " + ").asInstanceOf[Int] + b.toString).asInstanceOf[Int] + " = ").asInstanceOf[Int] + a + b.toString)
    println((((a.toString + " - ").asInstanceOf[Int] + b.toString).asInstanceOf[Int] + " = ").asInstanceOf[Int] + a - b.toString)
    println((((a.toString + " * ").asInstanceOf[Int] + b.toString).asInstanceOf[Int] + " = ").asInstanceOf[Int] + a * b.toString)
    println((((a.toString + " quo ").asInstanceOf[Int] + b.toString).asInstanceOf[Int] + " = ").asInstanceOf[Int] + (a / b).toInt.toString)
    println((((a.toString + " rem ").asInstanceOf[Int] + b.toString).asInstanceOf[Int] + " = ").asInstanceOf[Int] + a % b.toString)
  }
  
  def main(args: Array[String]): Unit = {
    main()
  }
}
