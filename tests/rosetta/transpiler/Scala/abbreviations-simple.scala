// Generated by Mochi v0.10.40 on 2025-07-25 19:19:47 GMT+7
import scala.collection.mutable.{ArrayBuffer, Map}
import scala.collection.immutable.ListMap
import scala.util.control.Breaks
import scala.util.control.Breaks._
object Main {
  private var _nowSeed: Long = 0L
  private var _nowSeeded: Boolean = false
  private def _now(): Int = {
    if (!_nowSeeded) {
      sys.env.get("MOCHI_NOW_SEED").foreach { s =>
      try { _nowSeed = s.toInt; _nowSeeded = true } catch { case _ : NumberFormatException => () }
    }
  }
  if (_nowSeeded) {
    _nowSeed = (_nowSeed * 1664525 + 1013904223) % 2147483647
    _nowSeed.toInt
  } else {
    Math.abs((System.nanoTime() / 1000).toInt)
  }
}

def toJson(value: Any, indent: Int = 0): String = value match {
  case m: scala.collection.Map[_, _] =>
  val items = ListMap(m.toSeq.sortBy(_._1.toString): _*).toSeq.map{ case (k,v) => "  "*(indent+1)+"\""+k.toString+"\": "+toJson(v, indent+1) }
  "{\n"+items.mkString(",\n")+"\n"+"  "*indent+"}"
  case s: Seq[_] =>
  val items = s.map(x => "  "*(indent+1)+toJson(x, indent+1))
  "[\n"+items.mkString(",\n")+"\n"+"  "*indent+"]"
  case s: String => "\""+s+"\""
  case other => other.toString
}

def main(args: Array[String]): Unit = {
  {
    System.gc()
    val _startMem = Runtime.getRuntime.totalMemory() - Runtime.getRuntime.freeMemory()
    val _start = _now()
    def fields(s: String): ArrayBuffer[String] = {
      var words: ArrayBuffer[String] = ArrayBuffer()
      var cur: String = ""
      var i: Int = 0
      while (i < (s).size) {
        val ch: String = s.substring(i, i + 1)
        if (((ch == " " || ch == "\n").asInstanceOf[Boolean] || ch == "\t").asInstanceOf[Boolean]) {
          if ((cur).size > 0) {
            words = words :+ cur
            cur = ""
          }
        } else {
          cur = (cur + ch).asInstanceOf[String]
        }
        i = (i + 1).asInstanceOf[Int]
      }
      if ((cur).size > 0) {
        words = words :+ cur
      }
      return words
    }
    def padRight(s: String, width: Int): String = {
      var out: String = s
      var i: Int = (s).size
      while (i < width) {
        out = out + " "
        i = (i + 1).asInstanceOf[Int]
      }
      return out
    }
    def join(xs: ArrayBuffer[String], sep: String): String = {
      var res: String = ""
      var i: Int = 0
      while (i < (xs).size) {
        if (i > 0) {
          res = (res + sep).asInstanceOf[String]
        }
        res = (res + xs(i)).asInstanceOf[String]
        i = (i + 1).asInstanceOf[Int]
      }
      return res
    }
    def parseIntStr(str: String): Int = {
      var i: Int = 0
      var neg: Boolean = false
      if (((str).size > 0 && str.slice(0, 1) == "-").asInstanceOf[Boolean]) {
        neg = true
        i = 1
      }
      var n: Int = 0
      val digits: Map[String,Int] = Map("0" -> (0), "1" -> (1), "2" -> (2), "3" -> (3), "4" -> (4), "5" -> (5), "6" -> (6), "7" -> (7), "8" -> (8), "9" -> (9))
      while (i < (str).size) {
        n = (n * 10 + digits.getOrElse(str.slice(i, i + 1), null.asInstanceOf[Int])).asInstanceOf[Int]
        i = (i + 1).asInstanceOf[Int]
      }
      if (neg) {
        n = (0 - n).asInstanceOf[Int]
      }
      return n
    }
    def isDigits(s: String): Boolean = {
      if ((s).size == 0) {
        return false
      }
      var i: Int = 0
      while (i < (s).size) {
        val ch: String = s.substring(i, i + 1)
        if ((ch < "0" || ch > "9").asInstanceOf[Boolean]) {
          return false
        }
        i = (i + 1).asInstanceOf[Int]
      }
      return true
    }
    def readTable(table: String): Map[String,Any] = {
      val toks: ArrayBuffer[String] = fields(table)
      var cmds: ArrayBuffer[String] = ArrayBuffer()
      var mins: ArrayBuffer[Int] = ArrayBuffer()
      var i: Int = 0
      while (i < (toks).size) {
        val cmd: String = toks(i)
        var minlen: Int = (cmd).size
        i = (i + 1).asInstanceOf[Int]
        if ((i < (toks).size && isDigits(toks(i))).asInstanceOf[Boolean]) {
          val num: Int = parseIntStr(toks(i))
          if ((num >= 1 && num < (cmd).size).asInstanceOf[Boolean]) {
            minlen = num
            i = (i + 1).asInstanceOf[Int]
          }
        }
        cmds = cmds :+ cmd
        mins = mins :+ minlen
      }
      return Map("commands" -> (cmds), "mins" -> (mins))
    }
    def validate(commands: ArrayBuffer[String], mins: ArrayBuffer[Int], words: ArrayBuffer[String]): ArrayBuffer[String] = {
      var results: ArrayBuffer[String] = ArrayBuffer()
      var wi: Int = 0
      val _br6 = new Breaks
      _br6.breakable {
        while (wi < (words).size) {
          val w: String = words(wi)
          var found: Boolean = false
          val wlen: Int = (w).size
          var ci: Int = 0
          val _br7 = new Breaks
          _br7.breakable {
            while (ci < (commands).size) {
              val cmd: String = commands(ci)
              if (((mins(ci) != 0 && wlen >= mins(ci)).asInstanceOf[Boolean] && wlen <= (cmd).size).asInstanceOf[Boolean]) {
                val c = cmd.toUpperCase()
                val ww = w.toUpperCase()
                if (c.substring(0, wlen) == ww) {
                  results = results :+ c
                  found = true
                  _br7.break()
                }
              }
              ci = (ci + 1).asInstanceOf[Int]
            }
          }
          if ((!found).asInstanceOf[Boolean]) {
            results = results :+ "*error*"
          }
          wi = (wi + 1).asInstanceOf[Int]
        }
      }
      return results
    }
    def main(): Unit = {
      val table: String = "" + "add 1  alter 3  backup 2  bottom 1  Cappend 2  change 1  Schange  Cinsert 2  Clast 3 " + "compress 4 copy 2 count 3 Coverlay 3 cursor 3  delete 3 Cdelete 2  down 1  duplicate " + "3 xEdit 1 expand 3 extract 3  find 1 Nfind 2 Nfindup 6 NfUP 3 Cfind 2 findUP 3 fUP 2 " + "forward 2  get  help 1 hexType 4  input 1 powerInput 3  join 1 split 2 spltJOIN load " + "locate 1 Clocate 2 lowerCase 3 upperCase 3 Lprefix 2  macro  merge 2 modify 3 move 2 " + "msg  next 1 overlay 1 parse preserve 4 purge 3 put putD query 1 quit  read recover 3 " + "refresh renum 3 repeat 3 replace 1 Creplace 2 reset 3 restore 4 rgtLEFT right 2 left " + "2  save  set  shift 2  si  sort  sos  stack 3 status 4 top  transfer 3  type 1  up 1 "
      val sentence: String = "riG   rePEAT copies  put mo   rest    types   fup.    6\npoweRin"
      val tbl: Map[String,Any] = readTable(table)
      val commands: ArrayBuffer[String] = (tbl.getOrElse("commands", null.asInstanceOf[Any])).asInstanceOf[ArrayBuffer[String]]
      val mins: ArrayBuffer[Int] = (tbl.getOrElse("mins", null.asInstanceOf[Any])).asInstanceOf[ArrayBuffer[Int]]
      val words: ArrayBuffer[String] = fields(sentence)
      val results: ArrayBuffer[String] = validate(commands, mins, words)
      var out1: String = "user words:"
      var k: Int = 0
      while (k < (words).size) {
        out1 = out1 + " "
        if (k < (words).size - 1) {
          out1 = (out1 + padRight(words(k), (results(k)).size)).asInstanceOf[String]
        } else {
          out1 = (out1 + words(k)).asInstanceOf[String]
        }
        k = (k + 1).asInstanceOf[Int]
      }
      println(out1)
      println("full words: " + join(results, " "))
    }
    main()
    val _end = _now()
    System.gc()
    val _endMem = Runtime.getRuntime.totalMemory() - Runtime.getRuntime.freeMemory()
    val _durUs = (_end - _start) / 1000
    var _memDiff = _endMem - _startMem
    if (_memDiff <= 0) _memDiff = _endMem
    println(toJson(Map("duration_us" -> _durUs, "memory_bytes" -> _memDiff, "name" -> "main")))
  }
}
}
