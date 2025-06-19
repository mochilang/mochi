object Main {
	def convert(s: String, numRows: Int): String = {
		if (((numRows <= 1) || (numRows >= s.length))) {
			return s
		}
		var rows: scala.collection.mutable.ArrayBuffer[String] = scala.collection.mutable.ArrayBuffer()
		var i = 0
		while ((i < numRows)) {
			rows = (rows ++ scala.collection.mutable.ArrayBuffer(""))
			i = (i + 1)
		}
		var curr = 0
		var step = 1
		for (ch <- s) {
			rows(curr) = (rows(curr) + ch)
			if ((curr == 0)) {
				step = 1
			} else 			if ((curr == (numRows - 1))) {
				step = (-1)
			}
			curr = (curr + step)
		}
		var result: String = ""
		for (row <- rows) {
			result = (result + row)
		}
		return result
	}
	
	def main(args: Array[String]): Unit = {
	}
}
