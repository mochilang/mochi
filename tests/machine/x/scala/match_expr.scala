object match_expr {
  val x = 2
  val label = x match {
    case 1 => "one"
    case 2 => "two"
    case 3 => "three"
    case _ => "unknown"
  }
  def main(args: Array[String]): Unit = {
    println(label)
  }
}
