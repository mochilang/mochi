object two_sum {
  def twoSum(nums: List[Int], target: Int): List[Int] = {
    val n = nums.length
    for(i <- 0 until n) {
      for(j <- i + 1 until n) {
        if ((nums(i) + nums(j)).asInstanceOf[Int] == target) {
          return List[Int](i, j)
        }
      }
    }
    return List[Int](-1, -1)
  }
  
  def main(args: Array[String]): Unit = {
    val result = twoSum(List[Int](2, 7, 11, 15), 9)
    println((result(0)))
    println((result(1)))
  }
}
