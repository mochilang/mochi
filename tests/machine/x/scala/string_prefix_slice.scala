object string_prefix_slice {
  val prefix = "fore"
  val s1 = "forest"
  def main(args: Array[String]): Unit = {
    println(s1.substring(0, prefix.length) == prefix)
    val s2 = "desert"
    println(s2.substring(0, prefix.length) == prefix)
  }
}
