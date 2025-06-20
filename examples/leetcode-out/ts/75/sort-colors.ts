// Generated by Mochi TypeScript compiler

function sortColors(nums: Array<number>) : Array<number> {
	let low: number = 0
	let mid: number = 0
	let high: number = (nums.length - 1)
	while ((mid <= high)) {
		if ((nums[mid] == 0)) {
			let temp: number = nums[low]
			nums[low] = nums[mid]
			nums[mid] = temp
			low = (low + 1)
			mid = (mid + 1)
		} else 		if ((nums[mid] == 1)) {
			mid = (mid + 1)
		} else {
			let temp: number = nums[mid]
			nums[mid] = nums[high]
			nums[high] = temp
			high = (high - 1)
		}
	}
	return nums
}

function example_1(): void {
	if (!(_equal(sortColors([2, 0, 2, 1, 1, 0]), [0, 0, 1, 1, 2, 2]))) { throw new Error('expect failed') }
}

function example_2(): void {
	if (!(_equal(sortColors([2, 0, 1]), [0, 1, 2]))) { throw new Error('expect failed') }
}

function single_zero(): void {
	if (!(_equal(sortColors([0]), [0]))) { throw new Error('expect failed') }
}

function single_one(): void {
	if (!(_equal(sortColors([1]), [1]))) { throw new Error('expect failed') }
}

function single_two(): void {
	if (!(_equal(sortColors([2]), [2]))) { throw new Error('expect failed') }
}

function main(): void {
	example_1()
	example_2()
	single_zero()
	single_one()
	single_two()
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

