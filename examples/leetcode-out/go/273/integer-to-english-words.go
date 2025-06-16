package main

func expect(cond bool) {
	if !cond { panic("expect failed") }
}

func convertBelow1000(n int) string {
	var ones []string = []string{"", "One", "Two", "Three", "Four", "Five", "Six", "Seven", "Eight", "Nine"}
	var teens []string = []string{"Ten", "Eleven", "Twelve", "Thirteen", "Fourteen", "Fifteen", "Sixteen", "Seventeen", "Eighteen", "Nineteen"}
	var tens []string = []string{"", "", "Twenty", "Thirty", "Forty", "Fifty", "Sixty", "Seventy", "Eighty", "Ninety"}
	var words string = ""
	var num int = n
	if (num >= 100) {
		var hundred int = (num / 100)
		words = words + ones[hundred] + " Hundred"
		num = (num % 100)
		if (num > 0) {
			words = words + " "
		}
	}
	if (num >= 20) {
		var t int = (num / 10)
		words = words + tens[t]
		num = (num % 10)
		if (num > 0) {
			words = words + " " + ones[num]
		}
	} else 	if (num >= 10) {
		words = words + teens[(num - 10)]
	} else 	if (num > 0) {
		words = words + ones[num]
	}
	return words
}

func numberToWords(num int) string {
	if (num == 0) {
		return "Zero"
	}
	var groups []string = []string{"", "Thousand", "Million", "Billion"}
	var parts []string = []string{}
	var n int = num
	var i int = 0
	for (n > 0) {
		var chunk int = (n % 1000)
		if (chunk > 0) {
			var chunkWords string = convertBelow1000(chunk)
			var part string = chunkWords
			if (groups[i] != "") {
				part = part + " " + groups[i]
			}
			parts = []string{part}
		}
		n = (n / 1000)
		i = (i + 1)
	}
	var result string = ""
	var j int = 0
	for (j < len(parts)) {
		result = result + parts[j]
		if (j < (len(parts) - 1)) {
			result = result + " "
		}
		j = (j + 1)
	}
	return result
}

func example_1() {
	expect((numberToWords(123) == "One Hundred Twenty Three"))
}

func example_2() {
	expect((numberToWords(12345) == "Twelve Thousand Three Hundred Forty Five"))
}

func example_3() {
	expect((numberToWords(1234567) == "One Million Two Hundred Thirty Four Thousand Five Hundred Sixty Seven"))
}

func zero() {
	expect((numberToWords(0) == "Zero"))
}

func million_and_tens() {
	expect((numberToWords(1000010) == "One Million Ten"))
}

func main() {
	example_1()
	example_2()
	example_3()
	zero()
	million_and_tens()
}

