object Main {
    def convert(s: String, numRows: Int): String = {
        if (((numRows <= 1) || (numRows >= s.length))) {
            return s
        }
        var rows = scala.collection.mutable.ArrayBuffer()
        var i: Int = 0
        while ((i < numRows)) {
            rows = (rows ++ scala.collection.mutable.ArrayBuffer(""))
            i = (i + 1)
        }
        var curr: Int = 0
        var step: Int = 1
        val it1 = s.iterator
        while (it1.hasNext) {
            val ch: String = it1.next().toString
            rows.update(curr, (_indexList(rows, curr) + ch))
            if ((curr == 0)) {
                step = 1
            } else             if ((curr == (numRows - 1))) {
                step = (-1)
            }
            curr = (curr + step)
        }
        var result: String = ""
        val it2 = rows.iterator
        while (it2.hasNext) {
            val row: String = it2.next().toString
            result = (result + row)
        }
        return result
    }
    
    def test_example_1(): Unit = {
        expect((convert("PAYPALISHIRING", 3) == "PAHNAPLSIIGYIR"))
    }
    
    def test_example_2(): Unit = {
        expect((convert("PAYPALISHIRING", 4) == "PINALSIGYAHRPI"))
    }
    
    def test_single_row(): Unit = {
        expect((convert("A", 1) == "A"))
    }
    
    def main(args: Array[String]): Unit = {
        test_example_1()
        test_example_2()
        test_single_row()
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

