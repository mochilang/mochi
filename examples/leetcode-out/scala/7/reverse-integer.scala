object Main {
    def reverse(x: Int): Int = {
        var sign: Int = 1
        var n: Int = x
        if ((n < 0)) {
            sign = (-1)
            n = (-n)
        }
        var rev: Int = 0
        while ((n != 0)) {
            val digit: Int = (n % 10)
            rev = ((rev * 10) + digit)
            n = (n / 10)
        }
        rev = (rev * sign)
        if (((rev < (((-2147483647) - 1))) || (rev > 2147483647))) {
            return 0
        }
        return rev
    }
    
    def test_example_1(): Unit = {
        expect((reverse(123) == 321))
    }
    
    def test_example_2(): Unit = {
        expect((reverse((-123)) == ((-321))))
    }
    
    def test_example_3(): Unit = {
        expect((reverse(120) == 21))
    }
    
    def test_overflow(): Unit = {
        expect((reverse(1534236469) == 0))
    }
    
    def main(args: Array[String]): Unit = {
        test_example_1()
        test_example_2()
        test_example_3()
        test_overflow()
    }
}
def expect(cond: Boolean): Unit = {
        if (!cond) throw new RuntimeException("expect failed")
}

