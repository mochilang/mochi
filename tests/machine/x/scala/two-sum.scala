object two-sum {
  def twoSum(nums: list[Int], target: Int): list[Int] = {
    val n = nums.length
    for(i <- 0 to n) {
      for(j <- i + 1 to n) {
        if (nums(i) + nums(j) == target) {
          return List(i, j)
        }
      }
    }
    return List(-1, -1)
  }
  
  def main(args: Array[String]): Unit = {
    val result = twoSum(List(2, 7, 11, 15), 9)
    println(result(0))
    println(result(1))
  }
}
