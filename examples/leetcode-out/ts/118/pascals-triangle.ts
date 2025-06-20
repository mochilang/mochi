// Generated by Mochi TypeScript compiler

function generateTriangle(numRows: number) : Array<Array<number>> {
	if ((numRows <= 0)) {
		return []
	}
	let result: Array<Array<number>> = [[1]]
	let i: number = 1
	while ((i < numRows)) {
		let prev: Array<number> = result[(i - 1)]
		let row: Array<number> = [1]
		let j: number = 1
		while ((j < prev.length)) {
			row = row.concat([(prev[(j - 1)] + prev[j])])
			j = (j + 1)
		}
		row = row.concat([1])
		result = result.concat([row])
		i = (i + 1)
	}
	return result
}

function example_1(): void {
	if (!(_equal(generateTriangle(5), [[1], [1, 1], [1, 2, 1], [1, 3, 3, 1], [1, 4, 6, 4, 1]]))) { throw new Error('expect failed') }
}

function example_2(): void {
	if (!(_equal(generateTriangle(1), [[1]]))) { throw new Error('expect failed') }
}

function zero_rows(): void {
	if (!(_equal(generateTriangle(0), []))) { throw new Error('expect failed') }
}

function main(): void {
	example_1()
	example_2()
	zero_rows()
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

