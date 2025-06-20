// Generated by Mochi TypeScript compiler

function fullJustify(words: Array<string>, maxWidth: number) : Array<string> {
	let result: Array<string> = []
	let i: number = 0
	while ((i < words.length)) {
		let j: number = i
		let lineLen: number = 0
		while ((j < words.length)) {
			if ((((lineLen + words[j].length) + ((j - i))) <= maxWidth)) {
				lineLen = (lineLen + words[j].length)
				j = (j + 1)
			} else {
				break
			}
		}
		let line: string = ""
		let numWords: number = (j - i)
		if (((j == words.length) || (numWords == 1))) {
			let k: number = i
			while ((k < j)) {
				line = line + words[k]
				if ((k < (j - 1))) {
					line = line + " "
				}
				k = (k + 1)
			}
			while ((line.length < maxWidth)) {
				line = line + " "
			}
		} else {
			let totalSpaces: number = (maxWidth - lineLen)
			let gaps: number = (numWords - 1)
			let spaceEach: number = Math.trunc(totalSpaces / gaps)
			let extra: number = (totalSpaces % gaps)
			let k: number = i
			while ((k < j)) {
				line = line + words[k]
				if ((k < (j - 1))) {
					let s: number = 0
					while ((s < spaceEach)) {
						line = line + " "
						s = (s + 1)
					}
					if ((extra > 0)) {
						line = line + " "
						extra = (extra - 1)
					}
				}
				k = (k + 1)
			}
		}
		result = result.concat([line])
		i = j
	}
	return result
}

function example_1(): void {
	let words: Array<string> = ["This", "is", "an", "example", "of", "text", "justification."]
	if (!(_equal(fullJustify(words, 16), ["This    is    an", "example  of text", "justification.  "]))) { throw new Error('expect failed') }
}

function example_2(): void {
	let words: Array<string> = ["What", "must", "be", "acknowledgment", "shall", "be"]
	if (!(_equal(fullJustify(words, 16), ["What   must   be", "acknowledgment  ", "shall be        "]))) { throw new Error('expect failed') }
}

function example_3(): void {
	let words: Array<string> = ["Science", "is", "what", "we", "understand", "well", "enough", "to", "explain", "to", "a", "computer.", "Art", "is", "everything", "else", "we", "do"]
	if (!(_equal(fullJustify(words, 20), ["Science  is  what we", "understand      well", "enough to explain to", "a  computer.  Art is", "everything  else  we", "do                  "]))) { throw new Error('expect failed') }
}

function main(): void {
	example_1()
	example_2()
	example_3()
}
function _equal(a: any, b: any): boolean {
  if (Array.isArray(a) && Array.isArray(b)) {
    if (a.length !== b.length) return false;
    for (let i = 0; i < a.length; i++) { if (!_equal(a[i], b[i])) return false; }
    return true;
  }
  if (a && b && typeof a === 'object' && typeof b === 'object') {
    const ak = Object.keys(a); const bk = Object.keys(b);
    if (ak.length !== bk.length) return false;
    for (const k of ak) { if (!bk.includes(k) || !_equal((a as any)[k], (b as any)[k])) return false; }
    return true;
  }
  return a === b;
}

main()

