object Main {
	def twoSum(nums: scala.collection.mutable.ArrayBuffer[Int], target: Int): scala.collection.mutable.ArrayBuffer[Int] = {
		val n = nums.length
		for (i <- 0 until n) {
			for (j <- (i + 1) until n) {
				if (((nums(i) + nums(j)) == target)) {
					return scala.collection.mutable.ArrayBuffer(i, j)
				}
			}
		}
		return scala.collection.mutable.ArrayBuffer((-1), (-1))
	}
	
	def main(args: Array[String]): Unit = {
		val result = twoSum(scala.collection.mutable.ArrayBuffer(2, 7, 11, 15), 9)
		println(result(0))
		println(result(1))
	}
}
