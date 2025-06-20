// Generated by Mochi TypeScript compiler

function rotateRight(nums: Array<number>, k: number) : Array<number> {
	let n: number = nums.length
	if ((n == 0)) {
		return nums
	}
	let r: number = (k % n)
	if ((r == 0)) {
		return nums
	}
	let result: Array<number> = []
	let i: number = 0
	while ((i < n)) {
		let idx: number = ((((n - r) + i)) % n)
		result = result.concat([nums[idx]])
		i = (i + 1)
	}
	return result
}

function example_1(): void {
	if (!(_equal(rotateRight([1, 2, 3, 4, 5], 2), [4, 5, 1, 2, 3]))) { throw new Error('expect failed') }
}

function example_2(): void {
	if (!(_equal(rotateRight([0, 1, 2], 4), [2, 0, 1]))) { throw new Error('expect failed') }
}

function k_is_zero(): void {
	if (!(_equal(rotateRight([1, 2, 3], 0), [1, 2, 3]))) { throw new Error('expect failed') }
}

function empty_list(): void {
	if (!(_equal(rotateRight([], 5), []))) { throw new Error('expect failed') }
}

function main(): void {
	example_1()
	example_2()
	k_is_zero()
	empty_list()
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

