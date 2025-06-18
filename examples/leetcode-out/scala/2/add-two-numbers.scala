object Main {
	def addTwoNumbers(l1: List[Int], l2: List[Int]): List[Int] = {
		var i = 0
		var j = 0
		var carry = 0
		var result: List[Int] = List()
		while ((((i < l1.length) || (j < l2.length)) || (carry > 0))) {
			var x = 0
			if ((i < l1.length)) {
				x = l1(i)
				i = (i + 1)
			}
			var y = 0
			if ((j < l2.length)) {
				y = l2(j)
				j = (j + 1)
			}
			val sum = ((x + y) + carry)
			val digit = (sum % 10)
			carry = (sum / 10)
			result = (result ++ List(digit))
		}
		return result
	}
	
	def main(args: Array[String]): Unit = {
	}
}
