package main

func expect(cond bool) {
	if !cond { panic("expect failed") }
}

func tenthLine(lines []string) string {
	if (len(lines) < 10) {
		return ""
	}
	return lines[9]
}

func has_tenth_line() {
	var lines []string = []string{"Line1", "Line2", "Line3", "Line4", "Line5", "Line6", "Line7", "Line8", "Line9", "Line10"}
	_ = lines
	expect((tenthLine(lines) == "Line10"))
}

func not_enough_lines() {
	var lines []string = []string{"a", "b", "c"}
	_ = lines
	expect((tenthLine(lines) == ""))
}

func main() {
	has_tenth_line()
	not_enough_lines()
}

