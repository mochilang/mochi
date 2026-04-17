import scala.io.Source

object Main extends App {
  val tokens = Source.stdin.mkString.split("\\s+").filter(_.nonEmpty).map(_.toInt)
  if (tokens.nonEmpty) {
    var idx = 0
    val t = tokens(idx)
    idx += 1

    def twoSum(nums: Array[Int], target: Int): (Int, Int) = {
      for (i <- nums.indices) {
        for (j <- i + 1 until nums.length) {
          if (nums(i) + nums(j) == target) {
            return (i, j)
          }
        }
      }
      (0, 0)
    }

    val out = collection.mutable.ArrayBuffer[String]()
    for (_ <- 0 until t) {
      val n = tokens(idx)
      val target = tokens(idx + 1)
      idx += 2
      val nums = tokens.slice(idx, idx + n)
      idx += n
      val ans = twoSum(nums, target)
      out += s"${ans._1} ${ans._2}"
    }
    print(out.mkString("\n"))
  }
}
