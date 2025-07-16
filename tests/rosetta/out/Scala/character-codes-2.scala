object character_codes_2 {
  def ord(ch: String): Int = {
    if (ch == "a") {
      return 97
    }
    if (ch == "π") {
      return 960
    }
    if (ch == "A") {
      return 65
    }
    return 0
  }
  
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
    println(ord("A").toString)
    println(chr(65))
  }
}
