object Main {
    def expand(s: String, left: Int, right: Int): Int = {
        var l: Int = left
        var r: Int = right
        val n: Int = s.length
        val brk1 = new scala.util.control.Breaks
        brk1.breakable {
            while (((l >= 0) && (r < n))) {
                if ((_indexString(s, l) != _indexString(s, r))) {
                    brk1.break()
                }
                l = (l - 1)
                r = (r + 1)
            }
        }
        return ((r - l) - 1)
    }
    
    def longestPalindrome(s: String): String = {
        if ((s.length <= 1)) {
            return s
        }
        var start: Int = 0
        var end: Int = 0
        val n: Int = s.length
        var i2 = 0
        while (i2 < n) {
            val i: Int = i2
            i2 = i2 + 1
            val len1: Int = expand(s, i, i)
            val len2: Int = expand(s, i, (i + 1))
            var l: Int = len1
            if ((len2 > len1)) {
                l = len2
            }
            if ((l > ((end - start)))) {
                start = (i - ((((l - 1)) / 2)))
                end = (i + ((l / 2)))
            }
        }
        return _sliceString(s, start, (end + 1))
    }
    
    def test_example_1(): Unit = {
        val ans: String = longestPalindrome("babad")
        expect(((ans == "bab") || (ans == "aba")))
    }
    
    def test_example_2(): Unit = {
        expect((longestPalindrome("cbbd") == "bb"))
    }
    
    def test_single_char(): Unit = {
        expect((longestPalindrome("a") == "a"))
    }
    
    def test_two_chars(): Unit = {
        val ans: String = longestPalindrome("ac")
        expect(((ans == "a") || (ans == "c")))
    }
    
    def main(args: Array[String]): Unit = {
        test_example_1()
        test_example_2()
        test_single_char()
        test_two_chars()
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

def _sliceString(s: String, i: Int, j: Int): String = {
        var start = i
        var end = j
        val chars = s.toVector
        val n = chars.length
        if (start < 0) start += n
        if (end < 0) end += n
        if (start < 0) start = 0
        if (end > n) end = n
        if (end < start) end = start
        chars.slice(start, end).mkString
}

