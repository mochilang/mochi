object abbreviations_simple {
  def fields(s: String): List[String] = {
    var words: List[String] = scala.collection.mutable.ArrayBuffer[Any]()
    var cur = ""
    var i = 0
    while (i < s.length) {
      val ch = s.substring(i, i + 1)
      if (ch == " " || ch == "\n" || ch == "\t") {
        if (cur.length > 0) {
          words = words :+ cur
          cur = ""
        }
      } else {
        cur += ch
      }
      i += 1
    }
    if (cur.length > 0) {
      words = words :+ cur
    }
    return words
  }
  
  def padRight(s: String, width: Int): String = {
    var out = s
    var i = s.length
    while (i < width) {
      out += " "
      i += 1
    }
    return out
  }
  
  def join(xs: List[String], sep: String): String = {
    var res = ""
    var i = 0
    while (i < xs.length) {
      if (i > 0) {
        res += sep
      }
      res += (xs).apply(i)
      i += 1
    }
    return res
  }
  
  def parseIntStr(str: String): Int = {
    var i = 0
    var neg = false
    if (str.length > 0 && str.substring(0, 1) == "-") {
      neg = true
      i = 1
    }
    var n = 0
    val digits = Map("0" -> 0, "1" -> 1, "2" -> 2, "3" -> 3, "4" -> 4, "5" -> 5, "6" -> 6, "7" -> 7, "8" -> 8, "9" -> 9)
    while (i < str.length) {
      n = n * 10 + (digits).apply(str.substring(i, i + 1))
      i += 1
    }
    if (neg) {
      n = -n
    }
    return n
  }
  
  def isDigits(s: String): Boolean = {
    if (s.length == 0) {
      return false
    }
    var i = 0
    while (i < s.length) {
      val ch = s.substring(i, i + 1)
      if (ch < "0" || ch > "9") {
        return false
      }
      i += 1
    }
    return true
  }
  
  def readTable(table: String): Map[String, any] = {
    val toks = fields(table)
    var cmds: List[String] = scala.collection.mutable.ArrayBuffer[Any]()
    var mins: List[Int] = scala.collection.mutable.ArrayBuffer[Any]()
    var i = 0
    while (i < toks.length) {
      val cmd = (toks).apply(i)
      var minlen = cmd.length
      i += 1
      if (i < toks.length && isDigits((toks).apply(i))) {
        val num = parseIntStr((toks).apply(i))
        if (num >= 1 && num < cmd.length) {
          minlen = num
          i += 1
        }
      }
      cmds = cmds :+ cmd
      mins = mins :+ minlen
    }
    return Map("commands" -> cmds, "mins" -> mins)
  }
  
  def validate(commands: List[String], mins: List[Int], words: List[String]): List[String] = {
    var results: List[String] = scala.collection.mutable.ArrayBuffer[Any]()
    var wi = 0
    while (wi < words.length) {
      val w = (words).apply(wi)
      var found = false
      val wlen = w.length
      var ci = 0
      while (ci < commands.length) {
        val cmd = (commands).apply(ci)
        if ((mins).apply(ci) != 0 && wlen >= (mins).apply(ci) && wlen <= cmd.length) {
          val c = upper(cmd)
          val ww = upper(w)
          if (c.substring(0, wlen) == ww) {
            results = results :+ c
            found = true
            return
          }
        }
        ci += 1
      }
      if (!found) {
        results = results :+ "*error*"
      }
      wi += 1
    }
    return results
  }
  
  def main() = {
    val table = ((((((("" + "add 1  alter 3  backup 2  bottom 1  Cappend 2  change 1  Schange  Cinsert 2  Clast 3 ").asInstanceOf[Int] + "compress 4 copy 2 count 3 Coverlay 3 cursor 3  delete 3 Cdelete 2  down 1  duplicate ").asInstanceOf[Int] + "3 xEdit 1 expand 3 extract 3  find 1 Nfind 2 Nfindup 6 NfUP 3 Cfind 2 findUP 3 fUP 2 ").asInstanceOf[Int] + "forward 2  get  help 1 hexType 4  input 1 powerInput 3  join 1 split 2 spltJOIN load ").asInstanceOf[Int] + "locate 1 Clocate 2 lowerCase 3 upperCase 3 Lprefix 2  macro  merge 2 modify 3 move 2 ").asInstanceOf[Int] + "msg  next 1 overlay 1 parse preserve 4 purge 3 put putD query 1 quit  read recover 3 ").asInstanceOf[Int] + "refresh renum 3 repeat 3 replace 1 Creplace 2 reset 3 restore 4 rgtLEFT right 2 left ").asInstanceOf[Int] + "2  save  set  shift 2  si  sort  sos  stack 3 status 4 top  transfer 3  type 1  up 1 "
    val sentence = "riG   rePEAT copies  put mo   rest    types   fup.    6\npoweRin"
    val tbl = readTable(table)
    val commands = (tbl).apply("commands").asInstanceOf[List[String]]
    val mins = (tbl).apply("mins").asInstanceOf[List[Int]]
    val words = fields(sentence)
    val results = validate(commands, mins, words)
    var out1 = "user words:"
    var k = 0
    while (k < words.length) {
      out1 += " "
      if (k < words.length - 1) {
        out1 += padRight((words).apply(k), (results).apply(k).length)
      } else {
        out1 += (words).apply(k)
      }
      k += 1
    }
    println(out1)
    println("full words: " + join(results, " "))
  }
  
  def main(args: Array[String]): Unit = {
    main()
  }
}
