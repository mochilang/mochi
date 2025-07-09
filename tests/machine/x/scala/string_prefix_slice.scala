object string_prefix_slice {
  def main(args: Array[String]): Unit = {
    val prefix = "fore"
    val s1 = "forest"
    println((s1.substring(0, prefix.length) == prefix))
    val s2 = "desert"
    println((s2.substring(0, prefix.length) == prefix))
  }
}
