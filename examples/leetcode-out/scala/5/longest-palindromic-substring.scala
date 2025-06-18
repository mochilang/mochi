object Main {
	def expand(s: String, left: Int, right: Int): Int = {
		var l = left
		var r = right
		val n = s.length
		while (((l >= 0) && (r < n))) {
			if ((s(l) != s(r))) {
			}
			l = (l - 1)
			r = (r + 1)
		}
		return ((r - l) - 1)
	}
	
	def longestPalindrome(s: String): String = {
		if ((s.length <= 1)) {
			return s
		}
		var start = 0
		var end = 0
		val n = s.length
		for (i <- 0 until n) {
			val len1 = expand(s, i, i)
			val len2 = expand(s, i, (i + 1))
			var l = len1
			if ((len2 > len1)) {
				l = len2
			}
			if ((l > ((end - start)))) {
				start = (i - ((((l - 1)) / 2)))
				end = (i + ((l / 2)))
			}
		}
		var res = ""
		var k = start
		while ((k <= end)) {
			res = (res + s(k))
			k = (k + 1)
		}
		return res
	}
	
	def main(args: Array[String]): Unit = {
	}
}
