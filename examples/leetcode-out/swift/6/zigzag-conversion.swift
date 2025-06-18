import Foundation

func convert(_ s: String, _ numRows: Int) -> String {
	let s = s
	let numRows = numRows
	
	if numRows <= 1 || numRows >= s.count {
		return s
	}
	var rows: [String] = []
	var i: Int = 0
	while i < numRows {
		rows = rows + [""]
		i = i + 1
	}
	var curr: Int = 0
	var step: Int = 1
	for ch_ch in s {
		let ch = String(ch_ch)
		rows[curr] = rows[curr] + ch
		if curr == 0 {
			step = 1
		} else 		if curr == numRows - 1 {
			step = -1
		}
		curr = curr + step
	}
	var result: String = ""
	for row in rows {
		result = result + row
	}
	return result
}

func main() {
}
main()
