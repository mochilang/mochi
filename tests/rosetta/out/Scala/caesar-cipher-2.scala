object caesar_cipher_2 {
  def indexOf(s: String, ch: String): Int = {
    var i = 0
    while (i < s.length) {
      if (s.substring(i, i + 1) == ch) {
        return i
      }
      i += 1
    }
    return -1
  }
  
  def ord(ch: String): Int = {
    val upper = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    val lower = "abcdefghijklmnopqrstuvwxyz"
    var idx = indexOf(upper, ch)
    if (idx >= 0) {
      return 65 + idx
    }
    idx = indexOf(lower, ch)
    if (idx >= 0) {
      return 97 + idx
    }
    return 0
  }
  
  def chr(n: Int): String = {
    val upper = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    val lower = "abcdefghijklmnopqrstuvwxyz"
    if (n >= 65 && n < 91) {
      return upper.substring(n - 65, n - 64)
    }
    if (n >= 97 && n < 123) {
      return lower.substring(n - 97, n - 96)
    }
    return "?"
  }
  
  def shiftRune(r: String, k: Int): String = {
    if (r >= "a" && r <= "z") {
      return chr(((ord(r) - 97 + k) % 26) + 97)
    }
    if (r >= "A" && r <= "Z") {
      return chr(((ord(r) - 65 + k) % 26) + 65)
    }
    return r
  }
  
  def encipher(s: String, k: Int): String = {
    var out = ""
    var i = 0
    while (i < s.length) {
      out += shiftRune(s.substring(i, i + 1), k)
      i += 1
    }
    return out
  }
  
  def decipher(s: String, k: Int): String = encipher(s, (26 - k % 26) % 26)
  
  def main() = {
    val pt = "The five boxing wizards jump quickly"
    println("Plaintext: " + pt)
    for(key <- List(0, 1, 7, 25, 26)) {
      if (key < 1 || key > 25) {
        println("Key " + key.toString + " invalid")
        // continue
      }
      val ct = encipher(pt, key)
      println("Key " + key.toString)
      println("  Enciphered: " + ct)
      println("  Deciphered: " + decipher(ct, key))
    }
  }
  
  def main(args: Array[String]): Unit = {
    main()
  }
}
