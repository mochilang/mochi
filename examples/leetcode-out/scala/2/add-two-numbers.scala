object Main {
    def addTwoNumbers(l1: scala.collection.mutable.ArrayBuffer[Int], l2: scala.collection.mutable.ArrayBuffer[Int]): scala.collection.mutable.ArrayBuffer[Int] = {
        var i: Int = 0
        var j: Int = 0
        var carry: Int = 0
        var result = scala.collection.mutable.ArrayBuffer()
        while ((((i < l1.length) || (j < l2.length)) || (carry > 0))) {
            var x: Int = 0
            if ((i < l1.length)) {
                x = _indexList(l1, i)
                i = (i + 1)
            }
            var y: Int = 0
            if ((j < l2.length)) {
                y = _indexList(l2, j)
                j = (j + 1)
            }
            val sum: Int = ((x + y) + carry)
            val digit: Int = (sum % 10)
            carry = (sum / 10)
            result = (result ++ scala.collection.mutable.ArrayBuffer(digit))
        }
        return result
    }
    
    def test_example_1(): Unit = {
        expect((addTwoNumbers(scala.collection.mutable.ArrayBuffer(2, 4, 3), scala.collection.mutable.ArrayBuffer(5, 6, 4)) == scala.collection.mutable.ArrayBuffer(7, 0, 8)))
    }
    
    def test_example_2(): Unit = {
        expect((addTwoNumbers(scala.collection.mutable.ArrayBuffer(0), scala.collection.mutable.ArrayBuffer(0)) == scala.collection.mutable.ArrayBuffer(0)))
    }
    
    def test_example_3(): Unit = {
        expect((addTwoNumbers(scala.collection.mutable.ArrayBuffer(9, 9, 9, 9, 9, 9, 9), scala.collection.mutable.ArrayBuffer(9, 9, 9, 9)) == scala.collection.mutable.ArrayBuffer(8, 9, 9, 9, 0, 0, 0, 1)))
    }
    
    def main(args: Array[String]): Unit = {
        test_example_1()
        test_example_2()
        test_example_3()
    }
}
def expect(cond: Boolean): Unit = {
        if (!cond) throw new RuntimeException("expect failed")
}

def _indexList[T](arr: scala.collection.mutable.ArrayBuffer[T], i: Int): T = {
        var idx = i
        val n = arr.length
        if (idx < 0) idx += n
        if (idx < 0 || idx >= n) throw new RuntimeException("index out of range")
        arr(idx)
}

