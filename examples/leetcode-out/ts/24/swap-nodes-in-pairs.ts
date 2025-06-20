// Generated by Mochi TypeScript compiler

function swapPairs(nums: Array<number>) : Array<number> {
	let i: number = 0
	let result: Array<any> = []
	while ((i < nums.length)) {
		if (((i + 1) < nums.length)) {
			result = result.concat([nums[(i + 1)], nums[i]])
		} else {
			result = result.concat([nums[i]])
		}
		i = (i + 2)
	}
	return result
}

function example_1(): void {
	if (!(_equal(swapPairs([1, 2, 3, 4]), [2, 1, 4, 3]))) { throw new Error('expect failed') }
}

function example_2(): void {
	if (!(_equal(swapPairs([]), []))) { throw new Error('expect failed') }
}

function example_3(): void {
	if (!(_equal(swapPairs([1]), [1]))) { throw new Error('expect failed') }
}

function odd_length(): void {
	if (!(_equal(swapPairs([1, 2, 3]), [2, 1, 3]))) { throw new Error('expect failed') }
}

function main(): void {
	example_1()
	example_2()
	example_3()
	odd_length()
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

