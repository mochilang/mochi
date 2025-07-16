object call_a_foreign_language_function {
  def strdup(s: String): String = s + ""
  
  def main() = {
    val go1 = "hello C"
    val c2 = strdup(go1)
    println(c2)
  }
  
  def main(args: Array[String]): Unit = {
    main()
  }
}
