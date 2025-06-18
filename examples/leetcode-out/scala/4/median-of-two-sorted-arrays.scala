object Main {
	def findMedianSortedArrays(nums1: List[Int], nums2: List[Int]): Double = {
		var merged: List[Int] = List()
		var i = 0
		var j = 0
		while (((i < nums1.length) || (j < nums2.length))) {
			if ((j >= nums2.length)) {
				merged = (merged ++ List(nums1(i)))
				i = (i + 1)
			} else 			if ((i >= nums1.length)) {
				merged = (merged ++ List(nums2(j)))
				j = (j + 1)
			} else 			if ((nums1(i) <= nums2(j))) {
				merged = (merged ++ List(nums1(i)))
				i = (i + 1)
			} else {
				merged = (merged ++ List(nums2(j)))
				j = (j + 1)
			}
		}
		val total = merged.length
		if (((total % 2) == 1)) {
			return merged((total / 2)).asInstanceOf[Double]
		}
		val mid1 = merged(((total / 2) - 1))
		val mid2 = merged((total / 2))
		return (((mid1 + mid2)).asInstanceOf[Double] / 2)
	}
	
	def main(args: Array[String]): Unit = {
	}
}
