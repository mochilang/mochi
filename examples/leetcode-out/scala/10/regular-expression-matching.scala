object Main {
	def isMatch(s: String, p: String): Boolean = {
		val m = s.length
		val n = p.length
		var dp: scala.collection.mutable.ArrayBuffer[scala.collection.mutable.ArrayBuffer[Boolean]] = scala.collection.mutable.ArrayBuffer()
		var i = 0
		while ((i <= m)) {
			var row: scala.collection.mutable.ArrayBuffer[Boolean] = scala.collection.mutable.ArrayBuffer()
			var j = 0
			while ((j <= n)) {
				row = (row ++ scala.collection.mutable.ArrayBuffer(false))
				j = (j + 1)
			}
			dp = (dp ++ scala.collection.mutable.ArrayBuffer(row))
			i = (i + 1)
		}
		dp(m)(n) = true
		var i2 = m
		while ((i2 >= 0)) {
			var j2 = (n - 1)
			while ((j2 >= 0)) {
				var first = false
				if ((i2 < m)) {
					if ((((p(j2) == s(i2))) || ((p(j2) == ".")))) {
						first = true
					}
				}
				if ((((j2 + 1) < n) && (p((j2 + 1)) == "*"))) {
					if ((dp(i2)((j2 + 2)) || ((first && dp((i2 + 1))(j2))))) {
						dp(i2)(j2) = true
					} else {
						dp(i2)(j2) = false
					}
				} else {
					if ((first && dp((i2 + 1))((j2 + 1)))) {
						dp(i2)(j2) = true
					} else {
						dp(i2)(j2) = false
					}
				}
				j2 = (j2 - 1)
			}
			i2 = (i2 - 1)
		}
		return dp(0)(0)
	}
	
	def main(args: Array[String]): Unit = {
	}
}
