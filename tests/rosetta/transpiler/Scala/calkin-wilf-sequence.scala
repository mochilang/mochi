// Generated by Mochi v0.10.40 on 2025-07-26 09:20:06 GMT+7
import scala.collection.mutable.{ArrayBuffer, Map}
import scala.math.BigInt
import scala.collection.immutable.ListMap
import scala.util.control.Breaks
import scala.util.control.Breaks._
import scala.annotation.tailrec
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

class BigRat(var num: BigInt, var den: BigInt) {
  def +(o: BigRat) = BigRat(num * o.den + o.num * den, den * o.den)
  def -(o: BigRat) = BigRat(num * o.den - o.num * den, den * o.den)
  def *(o: BigRat) = BigRat(num * o.num, den * o.den)
  def /(o: BigRat) = BigRat(num * o.den, den * o.num)
}
object BigRat {
  def apply(n: BigInt, d: BigInt = BigInt(1)): BigRat = {
    val g = n.gcd(d); var nn = n / g; var dd = d / g; if (dd < 0) { nn = -nn; dd = -dd }
    new BigRat(nn, dd)
  }
}
def _bigrat(n: BigInt, d: BigInt = BigInt(1)) = BigRat(n, d)
def num(r: BigRat): BigInt = r.num
def denom(r: BigRat): BigInt = r.den

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

private def _padStart(s: String, width: Int, pad: String): String = {
  var out = s
  while (out.length < width) { out = pad + out }
  out
}

private def _repeat(s: String, n: BigInt): String = s * n.toInt

private def _parseIntStr(s: String, base: BigInt): BigInt = BigInt(s, base.toInt)

def main(args: Array[String]): Unit = {
  {
    System.gc()
    val _startMem = Runtime.getRuntime.totalMemory() - Runtime.getRuntime.freeMemory()
    val _start = _now()
    def bigrat(a: BigInt, b: BigInt): BigRat = {
      return (_bigrat(a) / _bigrat(b)).asInstanceOf[BigRat]
    }
    def calkinWilf(n: BigInt): ArrayBuffer[BigRat] = {
      var seq: ArrayBuffer[BigRat] = ArrayBuffer()
      seq = seq :+ bigrat(BigInt(1), BigInt(1))
      var i: BigInt = BigInt(1)
      while (i < n) {
        var prev: BigRat = seq((i - BigInt(1)).toInt)
        val a: BigInt = num(prev)
        val b: BigInt = denom(prev)
        val f = a / b
        var t: BigRat = bigrat(f, BigInt(1))
        t = (t * _bigrat(BigInt(2))).asInstanceOf[BigRat]
        t = (t - prev).asInstanceOf[BigRat]
        t = (t + _bigrat(BigInt(1))).asInstanceOf[BigRat]
        t = (_bigrat(BigInt(1)) / t).asInstanceOf[BigRat]
        seq = seq :+ t
        i = (i + BigInt(1)).asInstanceOf[BigInt]
      }
      return seq
    }
    def toContinued(r: BigRat): ArrayBuffer[BigInt] = {
      var a: BigInt = num(r)
      var b: BigInt = denom(r)
      var res: ArrayBuffer[BigInt] = ArrayBuffer()
      val _br1 = new Breaks
      _br1.breakable {
        while (true) {
          res = res :+ a / b
          val t = a % b
          a = b
          b = t
          if (a == BigInt(1)) {
            _br1.break()
          }
        }
      }
      if ((res).size % BigInt(2) == BigInt(0)) {
        res(((res).size - BigInt(1)).toInt) = res(((res).size - BigInt(1)).toInt) - BigInt(1)
        res = res :+ BigInt(1)
      }
      return res
    }
    def termNumber(cf: ArrayBuffer[BigInt]): BigInt = {
      var b: String = ""
      var d: String = "1"
      for (n <- cf) {
        b = (_repeat(d, n) + b).toString
        if (d == "1") {
          d = "0"
        } else {
          d = "1"
        }
      }
      return (_parseIntStr(b, BigInt(2))).asInstanceOf[BigInt]
    }
    def commatize(n: BigInt): String = {
      var s = String.valueOf(n)
      var out: String = ""
      var i: BigInt = BigInt(0)
      var cnt: BigInt = BigInt(0)
      var neg: Boolean = false
      if (s.slice((BigInt(0)).toInt, (BigInt(1)).toInt) == "-") {
        neg = true
        s = s.slice((BigInt(1)).toInt, ((s).size).toInt)
      }
      i = (s).size - BigInt(1)
      while (i >= BigInt(0)) {
        out = s.slice((i).toInt, (i + BigInt(1)).toInt) + out
        cnt = (cnt + BigInt(1)).asInstanceOf[BigInt]
        if ((cnt == BigInt(3) && i != BigInt(0)).asInstanceOf[Boolean]) {
          out = "," + out
          cnt = BigInt(0)
        }
        i = (i - BigInt(1)).asInstanceOf[BigInt]
      }
      if (neg) {
        out = "-" + out
      }
      return out
    }
    def main(): Any = {
      val cw: ArrayBuffer[BigRat] = calkinWilf(BigInt(20))
      println("The first 20 terms of the Calkin-Wilf sequnence are:")
      var i: BigInt = BigInt(0)
      while (i < BigInt(20)) {
        val r: BigRat = cw((i).toInt)
        val s = String.valueOf(num(r))
        if (denom(r) != BigInt(1)) {
          s = s + "/" + String.valueOf(denom(r))
        }
        println(_padStart(i + BigInt(1), BigInt(2), " ") + ": " + s)
        i = (i + BigInt(1)).asInstanceOf[BigInt]
      }
      val r: BigRat = bigrat(BigInt(83116), BigInt(51639))
      val cf: ArrayBuffer[BigInt] = toContinued(r)
      val tn: BigInt = termNumber(cf)
      println("" + String.valueOf(num(r)) + "/" + String.valueOf(denom(r)) + " is the " + commatize(tn) + "th term of the sequence.")
    }
    main()
    val _end = _now()
    System.gc()
    val _endMem = Runtime.getRuntime.totalMemory() - Runtime.getRuntime.freeMemory()
    val _durUs = (_end - _start) / 1000
    var _memDiff = _endMem - _startMem
    if (_memDiff <= 0) _memDiff = _endMem
    println(toJson(scala.collection.immutable.Map("duration_us" -> _durUs, "memory_bytes" -> _memDiff, "name" -> "main")))
  }
}
}
