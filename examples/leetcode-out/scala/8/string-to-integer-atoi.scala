object Main {
    def digit(ch: String): Int = {
        if ((ch == "0")) {
            return 0
        }
        if ((ch == "1")) {
            return 1
        }
        if ((ch == "2")) {
            return 2
        }
        if ((ch == "3")) {
            return 3
        }
        if ((ch == "4")) {
            return 4
        }
        if ((ch == "5")) {
            return 5
        }
        if ((ch == "6")) {
            return 6
        }
        if ((ch == "7")) {
            return 7
        }
        if ((ch == "8")) {
            return 8
        }
        if ((ch == "9")) {
            return 9
        }
        return (-1)
    }
    
    def myAtoi(s: String): Int = {
        var i: Int = 0
        val n: Int = s.length
        while (((i < n) && (_indexString(s, i) == _indexString(" ", 0)))) {
            i = (i + 1)
        }
        var sign: Int = 1
        if (((i < n) && (((_indexString(s, i) == _indexString("+", 0)) || (_indexString(s, i) == _indexString("-", 0)))))) {
            if ((_indexString(s, i) == _indexString("-", 0))) {
                sign = (-1)
            }
            i = (i + 1)
        }
        var result: Int = 0
        val brk1 = new scala.util.control.Breaks
        brk1.breakable {
            while ((i < n)) {
                val ch: String = _sliceString(s, i, (i + 1))
                val d: Int = digit(ch)
                if ((d < 0)) {
                    brk1.break()
                }
                result = ((result * 10) + d)
                i = (i + 1)
            }
        }
        result = (result * sign)
        if ((result > 2147483647)) {
            return 2147483647
        }
        if ((result < ((-2147483648)))) {
            return (-2147483648)
        }
        return result
    }
    
    def test_example_1(): Unit = {
        expect((myAtoi("42") == 42))
    }
    
    def test_example_2(): Unit = {
        expect((myAtoi("   -42") == ((-42))))
    }
    
    def test_example_3(): Unit = {
        expect((myAtoi("4193 with words") == 4193))
    }
    
    def test_example_4(): Unit = {
        expect((myAtoi("words and 987") == 0))
    }
    
    def test_example_5(): Unit = {
        expect((myAtoi("-91283472332") == ((-2147483648))))
    }
    
    def main(args: Array[String]): Unit = {
        test_example_1()
        test_example_2()
        test_example_3()
        test_example_4()
        test_example_5()
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

