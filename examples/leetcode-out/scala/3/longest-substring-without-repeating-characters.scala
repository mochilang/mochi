object Main {
    def lengthOfLongestSubstring(s: String): Int = {
        val n: Int = s.length
        var start: Int = 0
        var best: Int = 0
        var i: Int = 0
        val brk1 = new scala.util.control.Breaks
        brk1.breakable {
            while ((i < n)) {
                var j: Int = start
                val brk2 = new scala.util.control.Breaks
                brk2.breakable {
                    while ((j < i)) {
                        if ((_indexString(s, j) == _indexString(s, i))) {
                            start = (j + 1)
                            brk2.break()
                        }
                        j = (j + 1)
                    }
                }
                val length: Int = ((i - start) + 1)
                if ((length > best)) {
                    best = length
                }
                i = (i + 1)
            }
        }
        return best
    }
    
    def test_example_1(): Unit = {
        expect((lengthOfLongestSubstring("abcabcbb") == 3))
    }
    
    def test_example_2(): Unit = {
        expect((lengthOfLongestSubstring("bbbbb") == 1))
    }
    
    def test_example_3(): Unit = {
        expect((lengthOfLongestSubstring("pwwkew") == 3))
    }
    
    def test_empty_string(): Unit = {
        expect((lengthOfLongestSubstring("") == 0))
    }
    
    def main(args: Array[String]): Unit = {
        test_example_1()
        test_example_2()
        test_example_3()
        test_empty_string()
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

