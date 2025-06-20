// Generated by Mochi TypeScript compiler

function ord(ch: string) : number {
	let letters: Record<string, number> = {"a": 0, "b": 1, "c": 2, "d": 3, "e": 4, "f": 5, "g": 6, "h": 7, "i": 8, "j": 9, "k": 10, "l": 11, "m": 12, "n": 13, "o": 14, "p": 15, "q": 16, "r": 17, "s": 18, "t": 19, "u": 20, "v": 21, "w": 22, "x": 23, "y": 24, "z": 25}
	if (Object.prototype.hasOwnProperty.call(letters, String(ch))) {
		return letters[ch]
	}
	return 0
}

function patternKey(s: string) : string {
	if ((s.length == 0)) {
		return ""
	}
	let key: string = ""
	let base: number = ord(s[0])
	let i: number = 0
	while ((i < s.length)) {
		let diff: number = ((((ord(s[i]) - base) + 26)) % 26)
		key = key + String(diff) + ","
		i = (i + 1)
	}
	return key
}

function groupStrings(strings: Array<string>) : Array<Array<string>> {
	let groups: Record<string, Array<string>> = {}
	for (const s of strings) {
		let k: string = patternKey(s)
		let lst: Array<string> = []
		if (Object.prototype.hasOwnProperty.call(groups, String(k))) {
			lst = groups[k]
		}
		lst = lst.concat([s])
		groups[k] = lst
	}
	let result: Array<Array<string>> = []
	for (const k of Object.keys(groups)) {
		result = result.concat([groups[k]])
	}
	return result
}

function example_1(): void {
	let input: Array<string> = ["abc", "bcd", "acef", "xyz", "az", "ba", "a", "z"]
	let res: Array<Array<string>> = groupStrings(input)
	if (!((res.length == 4))) { throw new Error('expect failed') }
}

function single(): void {
	if (!((groupStrings(["a"])[0][0] == "a"))) { throw new Error('expect failed') }
}

function empty_list(): void {
	let res: Array<Array<string>> = groupStrings([])
	if (!((res.length == 0))) { throw new Error('expect failed') }
}

function main(): void {
	example_1()
	single()
	empty_list()
}
main()

