object match_full {
  val x = 2
  val label = x match {
    case 1 => "one"
    case 2 => "two"
    case 3 => "three"
    case _ => "unknown"
  }
  val day = "sun"
  val mood = day match {
    case "mon" => "tired"
    case "fri" => "excited"
    case "sun" => "relaxed"
    case _ => "normal"
  }
  val ok = true
  val status = ok match {
    case true => "confirmed"
    case false => "denied"
  }
  def classify(n: Int): String = {
    return n match {
      case 0 => "zero"
      case 1 => "one"
      case _ => "many"
    }
  }
  
  def main(args: Array[String]): Unit = {
    println((label))
    println((mood))
    println((status))
    println((classify(0)))
    println((classify(5)))
  }
}
