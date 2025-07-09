object match_expr {
  def main(args: Array[String]): Unit = {
    val x = 2
    val label = x match {
      case 1 => "one"
      case 2 => "two"
      case 3 => "three"
      case _ => "unknown"
    }
    println((label))
  }
}
