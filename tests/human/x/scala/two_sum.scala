object two_sum {
  def twoSum(nums: Array[Int], target: Int): Array[Int] = {
    val n = nums.length
    for(i <- 0 until n; j <- i+1 until n) {
      if(nums(i) + nums(j) == target) return Array(i,j)
    }
    Array(-1,-1)
  }
  def main(args: Array[String]): Unit = {
    val result = twoSum(Array(2,7,11,15), 9)
    println(result(0))
    println(result(1))
  }
}
