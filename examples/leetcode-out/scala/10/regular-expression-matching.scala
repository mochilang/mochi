object Main {
	def isMatch(s: String, p: String): Boolean = {
		val m = s.length
		val n = p.length
		var memo: scala.collection.mutable.Map[Int, Boolean] = scala.collection.mutable.Map()
		def dfs(i: Int, j: Int): Boolean = {
			val key = ((i * ((n + 1))) + j)
			if (memo.contains(key)) {
				return memo(key)
			}
			if ((j == n)) {
				return (i == m)
			}
			var first = false
			if ((i < m)) {
				if ((((p(j) == s(i))) || ((p(j) == ".")))) {
					first = true
				}
			}
			var ans = false
			if (((j + 1) < n)) {
				if ((p((j + 1)) == "*")) {
					if (dfs(i, (j + 2))) {
						ans = true
					} else 					if ((first && dfs((i + 1), j))) {
						ans = true
					}
				} else {
					if ((first && dfs((i + 1), (j + 1)))) {
						ans = true
					}
				}
			} else {
				if ((first && dfs((i + 1), (j + 1)))) {
					ans = true
				}
			}
			memo(key) = ans
			return ans
		}
		return dfs(0, 0)
	}
	
	def main(args: Array[String]): Unit = {
	}
}
