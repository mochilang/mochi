// Generated by Mochi TypeScript compiler

function intToRoman(num: number) : string {
	let values: Array<number> = [1000, 900, 500, 400, 100, 90, 50, 40, 10, 9, 5, 4, 1]
	let symbols: Array<string> = ["M", "CM", "D", "CD", "C", "XC", "L", "XL", "X", "IX", "V", "IV", "I"]
	let result: string = ""
	let i: number = 0
	while ((num > 0)) {
		while ((num >= values[i])) {
			result = result + symbols[i]
			num = (num - values[i])
		}
		i = (i + 1)
	}
	return result
}

function example_1(): void {
	if (!((intToRoman(3) == "III"))) { throw new Error('expect failed') }
}

function example_2(): void {
	if (!((intToRoman(58) == "LVIII"))) { throw new Error('expect failed') }
}

function example_3(): void {
	if (!((intToRoman(1994) == "MCMXCIV"))) { throw new Error('expect failed') }
}

function small_numbers(): void {
	if (!((intToRoman(4) == "IV"))) { throw new Error('expect failed') }
	if (!((intToRoman(9) == "IX"))) { throw new Error('expect failed') }
}

function main(): void {
	example_1()
	example_2()
	example_3()
	small_numbers()
}
main()

