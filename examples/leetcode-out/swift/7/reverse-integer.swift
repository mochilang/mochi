import Foundation

func reverse(_ x: Int) -> Int {
	let x = x
	
	var sign: Int = 1
	var n: Int = x
	if n < 0 {
		sign = -1
		n = -n
	}
	var rev: Int = 0
	while n != 0 {
		let digit: Int = n % 10
		rev = rev * 10 + digit
		n = n / 10
	}
	rev = rev * sign
	if rev < (-2147483647 - 1) || rev > 2147483647 {
		return 0
	}
	return rev
}

func main() {
}
main()
