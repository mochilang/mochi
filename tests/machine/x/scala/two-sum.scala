object two_sum {
  def twoSum(nums: List[Int], target: Int): List[Int] = {
    val n = nums.length
    for(i <- 0 to n) {
      for(j <- (i).asInstanceOf[Int] + 1 to n) {
        if (((nums(i)).asInstanceOf[Int] + (nums(j)).asInstanceOf[Int]).asInstanceOf[Int] == (target).asInstanceOf[Int]) {
          return List[Any](i, j)
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
