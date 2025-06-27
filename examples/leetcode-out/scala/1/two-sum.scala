object Main {
    def twoSum(nums: scala.collection.mutable.ArrayBuffer[Int], target: Int): scala.collection.mutable.ArrayBuffer[Int] = {
        val n: Int = nums.length
        var i1 = 0
        while (i1 < n) {
            val i: Int = i1
            i1 = i1 + 1
            var i2 = (i + 1)
            while (i2 < n) {
                val j: Int = i2
                i2 = i2 + 1
                if (((_indexList(nums, i) + _indexList(nums, j)) == target)) {
                    return scala.collection.mutable.ArrayBuffer(i, j)
                }
            }
        }
        return scala.collection.mutable.ArrayBuffer((-1), (-1))
    }
    
    def main(args: Array[String]): Unit = {
        val result: scala.collection.mutable.ArrayBuffer[Int] = twoSum(scala.collection.mutable.ArrayBuffer(2, 7, 11, 15), 9)
        println(_indexList(result, 0))
        println(_indexList(result, 1))
    }
}
def _indexList[T](arr: scala.collection.mutable.ArrayBuffer[T], i: Int): T = {
        var idx = i
        val n = arr.length
        if (idx < 0) idx += n
        if (idx < 0 || idx >= n) throw new RuntimeException("index out of range")
        arr(idx)
}

