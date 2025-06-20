// Generated by Mochi TypeScript compiler

function canPermutePalindrome(s: string) : boolean {
	let counts: Record<string, number> = {}
	let i: number = 0
	while ((i < s.length)) {
		let ch: string = s[i]
		if (Object.prototype.hasOwnProperty.call(counts, String(ch))) {
			counts[ch] = (counts[ch] + 1)
		} else {
			counts[ch] = 1
		}
		i = (i + 1)
	}
	let oddCount: number = 0
	for (const key of Object.keys(counts)) {
		if (((counts[key] % 2) == 1)) {
			oddCount = (oddCount + 1)
			if ((oddCount > 1)) {
				return false
			}
		}
	}
	return true
}

function example_1(): void {
	if (!((canPermutePalindrome("code") == false))) { throw new Error('expect failed') }
}

function example_2(): void {
	if (!((canPermutePalindrome("aab") == true))) { throw new Error('expect failed') }
}

function example_3(): void {
	if (!((canPermutePalindrome("carerac") == true))) { throw new Error('expect failed') }
}

function empty_string(): void {
	if (!((canPermutePalindrome("") == true))) { throw new Error('expect failed') }
}

function single_char(): void {
	if (!((canPermutePalindrome("a") == true))) { throw new Error('expect failed') }
}

function two_odds(): void {
	if (!((canPermutePalindrome("abc") == false))) { throw new Error('expect failed') }
}

function main(): void {
	example_1()
	example_2()
	example_3()
	empty_string()
	single_char()
	two_odds()
}
main()

