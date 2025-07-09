object string_prefix_slice {
  val prefix = "fore"
  val s1 = "forest"
  val s2 = "desert"
  def main(args: Array[String]): Unit = {
    println((s1.substring(0, prefix.length) == prefix))
    println((s2.substring(0, prefix.length) == prefix))
  }
}
