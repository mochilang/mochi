import Foundation

func generateParenthesis(_ n: Int) -> [String] {
	let n = n
	
	var result: [String] = []
	func backtrack(_ current: String, _ open: Int, _ close: Int) -> Void {
		let current = current
		let open = open
		let close = close
		
		if current.count == n * 2 {
			result = result + [current]
		} else {
			if open < n {
				backtrack(current + "(", open + 1, close)
			}
			if close < open {
				backtrack(current + ")", open, close + 1)
			}
		}
	}
	backtrack("", 0, 0)
	return result
}

func main() {
}
main()
