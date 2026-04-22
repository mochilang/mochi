import scala.io.StdIn
import scala.collection.mutable.ArrayBuffer

object Main {
  final class MedianFinder {
    private val data = ArrayBuffer.empty[Int]

    def addNum(num: Int): Unit = {
      var lo = 0
      var hi = data.length
      while (lo < hi) {
        val mid = (lo + hi) / 2
        if (data(mid) < num) lo = mid + 1 else hi = mid
      }
      data.insert(lo, num)
    }

    def findMedian(): Double = {
      val n = data.length
      if (n % 2 == 1) data(n / 2).toDouble
      else (data(n / 2 - 1) + data(n / 2)) / 2.0
    }
  }

  def main(args: Array[String]): Unit = {
    val lines = Iterator.continually(StdIn.readLine()).takeWhile(_ != null).map(_.trim).filter(_.nonEmpty).toArray
    if (lines.isEmpty) return
    val t = lines(0).toInt
    var idx = 1
    val blocks = ArrayBuffer.empty[String]
    for (_ <- 0 until t) {
      val m = lines(idx).toInt
      idx += 1
      val mf = new MedianFinder
      val out = ArrayBuffer.empty[String]
      for (_ <- 0 until m) {
        val parts = lines(idx).split("\\s+")
        idx += 1
        if (parts(0) == "addNum") mf.addNum(parts(1).toInt)
        else out += f"${mf.findMedian()}%.1f"
      }
      blocks += out.mkString("\n")
    }
    print(blocks.mkString("\n\n"))
  }
}
