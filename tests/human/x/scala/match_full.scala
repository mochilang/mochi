object match_full {
  def classify(n: Int): String = n match {
    case 0 => "zero"
    case 1 => "one"
    case _ => "many"
  }

  def main(args: Array[String]): Unit = {
    val x = 2
    val label = x match {
      case 1 => "one"
      case 2 => "two"
      case 3 => "three"
      case _ => "unknown"
    }
    println(label)

    val day = "sun"
    val mood = day match {
      case "mon" => "tired"
      case "fri" => "excited"
      case "sun" => "relaxed"
      case _ => "normal"
    }
    println(mood)

    val ok = true
    val status = if(ok) "confirmed" else "denied"
    println(status)

    println(classify(0))
    println(classify(5))
  }
}
