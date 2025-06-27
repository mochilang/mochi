object Main {
    def isPalindrome(x: Int): Boolean = {
        if ((x < 0)) {
            return false
        }
        val s: String = x.toString()
        val n: Int = s.length
        var i1 = 0
        while (i1 < (n / 2)) {
            val i: Int = i1
            i1 = i1 + 1
            if ((_indexString(s, i) != _indexString(s, ((n - 1) - i)))) {
                return false
            }
        }
        return true
    }
    
    def test_example_1(): Unit = {
        expect((isPalindrome(121) == true))
    }
    
    def test_example_2(): Unit = {
        expect((isPalindrome((-121)) == false))
    }
    
    def test_example_3(): Unit = {
        expect((isPalindrome(10) == false))
    }
    
    def test_zero(): Unit = {
        expect((isPalindrome(0) == true))
    }
    
    def main(args: Array[String]): Unit = {
        test_example_1()
        test_example_2()
        test_example_3()
        test_zero()
    }
}
def expect(cond: Boolean): Unit = {
        if (!cond) throw new RuntimeException("expect failed")
}

def _indexString(s: String, i: Int): String = {
        var idx = i
        val chars = s.toVector
        if (idx < 0) idx += chars.length
        if (idx < 0 || idx >= chars.length) throw new RuntimeException("index out of range")
        chars(idx).toString
}

