object camel_case_and_snake_case {
  def trimSpace(s: String): String = {
    var start = 0
    while (start < s.length && s.substring(start, start + 1) == " ") {
      start += 1
    }
    var end = s.length
    while (end > start && s.substring(end - 1, end) == " ") {
      end -= 1
    }
    return s.substring(start, end)
  }
  
  def isUpper(ch: String): Boolean = ch >= "A" && ch <= "Z"
  
  def padLeft(s: String, w: Int): String = {
    var res = ""
    var n = w - s.length
    while (n > 0) {
      res += " "
      n -= 1
    }
    return res + s
  }
  
  def snakeToCamel(s: String): String = {
    s = trimSpace(s)
    var out = ""
    var up = false
    var i = 0
    while (i < s.length) {
      val ch = s.substring(i, i + 1)
      if (ch == "_" || ch == "-" || ch == " " || ch == ".") {
        up = true
        i += 1
        // continue
      }
      if (i == 0) {
        out += lower(ch)
        up = false
        i += 1
        // continue
      }
      if (up) {
        out += upper(ch)
        up = false
      } else {
        out += ch
      }
      i += 1
    }
    return out
  }
  
  def camelToSnake(s: String): String = {
    s = trimSpace(s)
    var out = ""
    var prevUnd = false
    var i = 0
    while (i < s.length) {
      val ch = s.substring(i, i + 1)
      if (ch == " " || ch == "-" || ch == ".") {
        if (!prevUnd && out.length > 0) {
          out += "_"
          prevUnd = true
        }
        i += 1
        // continue
      }
      if (ch == "_") {
        if (!prevUnd && out.length > 0) {
          out += "_"
          prevUnd = true
        }
        i += 1
        // continue
      }
      if (isUpper(ch)) {
        if (i > 0 && (!prevUnd)) {
          out += "_"
        }
        out += lower(ch)
        prevUnd = false
      } else {
        out += lower(ch)
        prevUnd = false
      }
      i += 1
    }
    var start = 0
    while (start < out.length && out.substring(start, start + 1) == "_") {
      start += 1
    }
    var end = out.length
    while (end > start && out.substring(end - 1, end) == "_") {
      end -= 1
    }
    out = out.substring(start, end)
    var res = ""
    var j = 0
    var lastUnd = false
    while (j < out.length) {
      val c = out.substring(j, j + 1)
      if (c == "_") {
        if (!lastUnd) {
          res += c
        }
        lastUnd = true
      } else {
        res += c
        lastUnd = false
      }
      j += 1
    }
    return res
  }
  
  def main() = {
    val samples = List("snakeCase", "snake_case", "snake-case", "snake case", "snake CASE", "snake.case", "variable_10_case", "variable10Case", "ɛrgo rE tHis", "hurry-up-joe!", "c://my-docs/happy_Flag-Day/12.doc", " spaces ")
    println("=== To snake_case ===")
    for(s <- samples) {
      println((padLeft(s, 34) + " => ").asInstanceOf[Int] + camelToSnake(s))
    }
    println("")
    println("=== To camelCase ===")
    for(s <- samples) {
      println((padLeft(s, 34) + " => ").asInstanceOf[Int] + snakeToCamel(s))
    }
  }
  
  def main(args: Array[String]): Unit = {
    main()
  }
}
