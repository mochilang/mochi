// Generated by Mochi TypeScript compiler

function removeNthFromEnd(nums: Array<number>, n: number) : Array<number> {
	let idx: number = (nums.length - n)
	let result: Array<any> = []
	let i: number = 0
	while ((i < nums.length)) {
		if ((i != idx)) {
			result = result.concat([nums[i]])
		}
		i = (i + 1)
	}
	return result
}

function example_1(): void {
	if (!(_equal(removeNthFromEnd([1, 2, 3, 4, 5], 2), [1, 2, 3, 5]))) { throw new Error('expect failed') }
}

function example_2(): void {
	if (!(_equal(removeNthFromEnd([1], 1), []))) { throw new Error('expect failed') }
}

function example_3(): void {
	if (!(_equal(removeNthFromEnd([1, 2], 1), [1]))) { throw new Error('expect failed') }
}

function remove_first(): void {
	if (!(_equal(removeNthFromEnd([7, 8, 9], 3), [8, 9]))) { throw new Error('expect failed') }
}

function remove_last(): void {
	if (!(_equal(removeNthFromEnd([7, 8, 9], 1), [7, 8]))) { throw new Error('expect failed') }
}

function main(): void {
	example_1()
	example_2()
	example_3()
	remove_first()
	remove_last()
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

