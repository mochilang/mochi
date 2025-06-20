// Generated by Mochi TypeScript compiler

function findRepeatedDnaSequences(s: string) : Array<string> {
	let n: number = s.length
	if ((n < 10)) {
		return []
	}
	let seen: Record<string, number> = {}
	let result: Array<string> = []
	let i: number = 0
	while (((i + 10) <= n)) {
		let sub: string = s.slice(i, (i + 10))
		let count: number = 0
		if (Object.prototype.hasOwnProperty.call(seen, String(sub))) {
			count = seen[sub]
		}
		count = (count + 1)
		seen[sub] = count
		if ((count == 2)) {
			result = result.concat([sub])
		}
		i = (i + 1)
	}
	return result
}

function example_1(): void {
	if (!(_equal(findRepeatedDnaSequences("AAAAACCCCCAAAAACCCCCCAAAAAGGGTTT"), ["AAAAACCCCC", "CCCCCAAAAA"]))) { throw new Error('expect failed') }
}

function example_2(): void {
	if (!(_equal(findRepeatedDnaSequences("AAAAAAAAAAAAA"), ["AAAAAAAAAA"]))) { throw new Error('expect failed') }
}

function no_repeats(): void {
	if (!(_equal(findRepeatedDnaSequences("ACGTACGTAC"), []))) { throw new Error('expect failed') }
}

function short_string(): void {
	if (!(_equal(findRepeatedDnaSequences("AAAAA"), []))) { throw new Error('expect failed') }
}

function main(): void {
	example_1()
	example_2()
	no_repeats()
	short_string()
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

