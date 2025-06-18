import Foundation

func addTwoNumbers(_ l1: [Int], _ l2: [Int]) -> [Int] {
	let l1 = l1
	let l2 = l2
	
	var i = 0
	var j = 0
	var carry = 0
	var result: [Int] = []
	while i < l1.count || j < l2.count || carry > 0 {
		var x = 0
		if i < l1.count {
			x = l1[i]
			i = i + 1
		}
		var y = 0
		if j < l2.count {
			y = l2[j]
			j = j + 1
		}
		let sum = x + y + carry
		let digit = sum % 10
		carry = sum / 10
		result = result + [digit]
	}
	return result
}

func main() {
}
main()
