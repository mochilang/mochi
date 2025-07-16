object abbreviations_easy {
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
  
  def validate(commands: List[String], words: List[String], mins: List[Int]): List[String] = {
    var results: List[String] = scala.collection.mutable.ArrayBuffer[Any]()
    if (words.length == 0) {
      return results
    }
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
    val table = ((((("Add ALTer  BAckup Bottom  CAppend Change SCHANGE  CInsert CLAst COMPress Copy " + "COUnt COVerlay CURsor DELete CDelete Down DUPlicate Xedit EXPand EXTract Find ").asInstanceOf[Int] + "NFind NFINDUp NFUp CFind FINdup FUp FOrward GET Help HEXType Input POWerinput ").asInstanceOf[Int] + " Join SPlit SPLTJOIN  LOAD  Locate CLocate  LOWercase UPPercase  LPrefix MACRO ").asInstanceOf[Int] + "MErge MODify MOve MSG Next Overlay PARSE PREServe PURge PUT PUTD  Query  QUIT ").asInstanceOf[Int] + "READ  RECover REFRESH RENum REPeat  Replace CReplace  RESet  RESTore  RGTLEFT ").asInstanceOf[Int] + "RIght LEft  SAVE  SET SHift SI  SORT  SOS  STAck STATus  TOP TRAnsfer TypeUp "
    val commands = fields(table)
    var mins: List[Int] = scala.collection.mutable.ArrayBuffer[Any]()
    var i = 0
    while (i < commands.length) {
      var count = 0
      var j = 0
      val cmd = (commands).apply(i)
      while (j < cmd.length) {
        val ch = cmd.substring(j, j + 1)
        if (ch >= "A" && ch <= "Z") {
          count += 1
        }
        j += 1
      }
      mins = mins :+ count
      i += 1
    }
    val sentence = "riG   rePEAT copies  put mo   rest    types   fup.    6       poweRin"
    val words = fields(sentence)
    val results = validate(commands, words, mins)
    var out1 = "user words:  "
    var k = 0
    while (k < words.length) {
      out1 = (out1 + padRight((words).apply(k), (results).apply(k).length)).asInstanceOf[Int] + " "
      k += 1
    }
    println(out1)
    println("full words:  " + join(results, " "))
  }
  
  def main(args: Array[String]): Unit = {
    main()
  }
}
