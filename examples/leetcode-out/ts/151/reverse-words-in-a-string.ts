// Generated by Mochi TypeScript compiler

function reverseWords(s: string) : string {
	let i: number = (s.length - 1)
	let words: Array<string> = []
	while ((i >= 0)) {
		while (((i >= 0) && (s[i] == " "))) {
			i = (i - 1)
		}
		if ((i < 0)) {
			break
		}
		let j: number = i
		while (((j >= 0) && (s[j] != " "))) {
			j = (j - 1)
		}
		words = words.concat([s.slice((j + 1), (i + 1))])
		i = j
	}
	let result: string = ""
	let k: number = 0
	while ((k < words.length)) {
		if ((k > 0)) {
			result = result + " "
		}
		result = result + words[k]
		k = (k + 1)
	}
	return result
}

function example_1(): void {
	if (!((reverseWords("the sky is blue") == "blue is sky the"))) { throw new Error('expect failed') }
}

function example_2(): void {
	if (!((reverseWords("  hello world  ") == "world hello"))) { throw new Error('expect failed') }
}

function example_3(): void {
	if (!((reverseWords("a good   example") == "example good a"))) { throw new Error('expect failed') }
}

function single_word(): void {
	if (!((reverseWords("hello") == "hello"))) { throw new Error('expect failed') }
}

function only_spaces(): void {
	if (!((reverseWords("    ") == ""))) { throw new Error('expect failed') }
}

function main(): void {
	example_1()
	example_2()
	example_3()
	single_word()
	only_spaces()
}
main()

