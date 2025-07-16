object call_a_function_5 {
  def doIt(p: Map[String, Int]): Int = {
    var b = 0
    if (p.contains("b")) {
      b = (p).apply("b")
    }
    return (p).apply("a") + b + (p).apply("c")
  }
  
  def main() = {
    var p: Map[String, Int] = scala.collection.mutable.Map()
    p("a") = 1
    p("c") = 9
    println(doIt(p).toString)
  }
  
  def main(args: Array[String]): Unit = {
    main()
  }
}
