// Generated by Mochi TypeScript compiler

function strStr(haystack: string, needle: string) : number {
	let n: number = haystack.length
	let m: number = needle.length
	if ((m == 0)) {
		return 0
	}
	if ((m > n)) {
		return (-1)
	}
	for (let i: number = 0; i < ((n - m) + 1); i++) {
		let j: number = 0
		while ((j < m)) {
			if ((haystack[(i + j)] != needle[j])) {
				break
			}
			j = (j + 1)
		}
		if ((j == m)) {
			return i
		}
	}
	return (-1)
}

function example_1(): void {
	if (!((strStr("sadbutsad", "sad") == 0))) { throw new Error('expect failed') }
}

function example_2(): void {
	if (!((strStr("leetcode", "leeto") == ((-1))))) { throw new Error('expect failed') }
}

function empty_needle(): void {
	if (!((strStr("abc", "") == 0))) { throw new Error('expect failed') }
}

function needle_at_end(): void {
	if (!((strStr("hello", "lo") == 3))) { throw new Error('expect failed') }
}

function main(): void {
	example_1()
	example_2()
	empty_needle()
	needle_at_end()
}
main()

