// Generated by Mochi TypeScript compiler

function containsNearbyDuplicate(nums: Array<number>, k: number) : boolean {
	let index: Record<number, number> = {}
	let i: number = 0
	while ((i < nums.length)) {
		let num: number = nums[i]
		if (Object.prototype.hasOwnProperty.call(index, String(num))) {
			let j: number = index[num]
			if (((i - j) <= k)) {
				return true
			}
		}
		index[num] = i
		i = (i + 1)
	}
	return false
}

function example_1(): void {
	if (!((containsNearbyDuplicate([1, 2, 3, 1], 3) == true))) { throw new Error('expect failed') }
}

function example_2(): void {
	if (!((containsNearbyDuplicate([1, 0, 1, 1], 1) == true))) { throw new Error('expect failed') }
}

function example_3(): void {
	if (!((containsNearbyDuplicate([1, 2, 3, 1, 2, 3], 2) == false))) { throw new Error('expect failed') }
}

function no_duplicates(): void {
	if (!((containsNearbyDuplicate([1, 2, 3, 4, 5], 3) == false))) { throw new Error('expect failed') }
}

function duplicate_at_distance_k(): void {
	if (!((containsNearbyDuplicate([1, 2, 3, 1], 2) == false))) { throw new Error('expect failed') }
}

function duplicate_with_k_zero(): void {
	if (!((containsNearbyDuplicate([1, 1], 0) == false))) { throw new Error('expect failed') }
}

function main(): void {
	example_1()
	example_2()
	example_3()
	no_duplicates()
	duplicate_at_distance_k()
	duplicate_with_k_zero()
}
main()

