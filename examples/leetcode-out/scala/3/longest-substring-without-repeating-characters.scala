object Main {
	def lengthOfLongestSubstring(s: String): Int = {
		val n = s.length
		var start = 0
		var best = 0
		var i = 0
		while ((i < n)) {
			var j = start
			while ((j < i)) {
				if ((s(j) == s(i))) {
					start = (j + 1)
				}
				j = (j + 1)
			}
			val length = ((i - start) + 1)
			if ((length > best)) {
				best = length
			}
			i = (i + 1)
		}
		return best
	}
	
	def main(args: Array[String]): Unit = {
	}
}
