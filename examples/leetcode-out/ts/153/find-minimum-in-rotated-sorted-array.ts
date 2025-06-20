// Generated by Mochi TypeScript compiler

function findMin(nums: Array<number>) : number {
	let left: number = 0
	let right: number = (nums.length - 1)
	while ((left < right)) {
		let mid: number = (left + Math.trunc(((right - left)) / 2))
		if ((nums[mid] > nums[right])) {
			left = (mid + 1)
		} else {
			right = mid
		}
	}
	return nums[left]
}

function example_1(): void {
	if (!((findMin([3, 4, 5, 1, 2]) == 1))) { throw new Error('expect failed') }
}

function example_2(): void {
	if (!((findMin([4, 5, 6, 7, 0, 1, 2]) == 0))) { throw new Error('expect failed') }
}

function example_3(): void {
	if (!((findMin([11, 13, 15, 17]) == 11))) { throw new Error('expect failed') }
}

function single_element(): void {
	if (!((findMin([5]) == 5))) { throw new Error('expect failed') }
}

function two_elements(): void {
	if (!((findMin([2, 1]) == 1))) { throw new Error('expect failed') }
}

function main(): void {
	example_1()
	example_2()
	example_3()
	single_element()
	two_elements()
}
main()

