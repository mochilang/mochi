// Generated by Mochi TypeScript compiler

function toList(s: string) : Array<string> {
	let out: Array<string> = []
	for (let i: number = 0; i < s.length; i++) {
		out = out.concat([s[i]])
	}
	return out
}

function fromList(arr: Array<string>) : string {
	let out: string = ""
	for (const ch of arr) {
		out = out + ch
	}
	return out
}

function reverseString(chars: Array<string>) : Array<string> {
	let arr: Array<string> = chars
	let left: number = 0
	let right: number = (arr.length - 1)
	while ((left < right)) {
		let temp: string = arr[left]
		arr[left] = arr[right]
		arr[right] = temp
		left = (left + 1)
		right = (right - 1)
	}
	return arr
}

function example_1(): void {
	let input: Array<string> = toList("hello")
	let result: Array<string> = reverseString(input)
	if (!((fromList(result) == "olleh"))) { throw new Error('expect failed') }
}

function example_2(): void {
	let input: Array<string> = toList("Hannah")
	let result: Array<string> = reverseString(input)
	if (!((fromList(result) == "hannaH"))) { throw new Error('expect failed') }
}

function empty(): void {
	let input: Array<string> = []
	let result: Array<string> = reverseString(input)
	if (!((result.length == 0))) { throw new Error('expect failed') }
}

function main(): void {
	example_1()
	example_2()
	empty()
}
main()

