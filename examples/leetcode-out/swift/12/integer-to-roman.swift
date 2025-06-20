import Foundation

func intToRoman(_ num: Int) -> String {
	var num = num
	
	let values = [1000, 900, 500, 400, 100, 90, 50, 40, 10, 9, 5, 4, 1]
	let symbols = ["M", "CM", "D", "CD", "C", "XC", "L", "XL", "X", "IX", "V", "IV", "I"]
	var result = ""
	var i = 0
	while num > 0 {
		while num >= values[i] {
			result = result + symbols[i]
			num = num - values[i]
		}
		i = i + 1
	}
	return result
}

func main() {
}
main()
