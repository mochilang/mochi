object character_codes_4 {
  def chr(n: Int): String = {
    if (n == 97) {
      return "a"
    }
    if (n == 960) {
      return "π"
    }
    if (n == 65) {
      return "A"
    }
    return "?"
  }
  
  def main(args: Array[String]): Unit = {
    var b = 97
    var r = 960
    println(chr(97) + " " + chr(960))
    println(chr(b) + " " + chr(r))
  }
}
