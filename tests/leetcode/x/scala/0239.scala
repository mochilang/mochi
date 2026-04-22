import scala.collection.mutable.ArrayDeque

object Main {
  def solve(nums: Array[Int], k: Int): Array[Int] = {
    val dq = ArrayDeque[Int]()
    val ans = scala.collection.mutable.ArrayBuffer[Int]()
    for (i <- nums.indices) {
      while (dq.nonEmpty && dq.head <= i - k) dq.removeHead()
      while (dq.nonEmpty && nums(dq.last) <= nums(i)) dq.removeLast()
      dq.append(i)
      if (i >= k - 1) ans += nums(dq.head)
    }
    ans.toArray
  }

  def main(args: Array[String]): Unit = {
    val toks = io.Source.stdin.getLines().flatMap(_.trim.split("\\s+")).filter(_.nonEmpty).toArray
    if (toks.isEmpty) return
    var idx = 0
    val t = toks(idx).toInt
    idx += 1
    val blocks = new scala.collection.mutable.ArrayBuffer[String]()
    for (_ <- 0 until t) {
      val n = toks(idx).toInt
      idx += 1
      val nums = Array.tabulate(n)(_ => { val v = toks(idx).toInt; idx += 1; v })
      val k = toks(idx).toInt
      idx += 1
      val ans = solve(nums, k)
      blocks += ((Array(ans.length.toString) ++ ans.map(_.toString)).mkString("\n"))
    }
    print(blocks.mkString("\n\n"))
  }
}
