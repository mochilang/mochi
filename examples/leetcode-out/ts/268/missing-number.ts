// Generated by Mochi TypeScript compiler

function missingNumber(nums: Array<number>) : number {
	let n: number = nums.length
	let sum: number = 0
	for (const num of nums) {
		sum = (sum + num)
	}
	let expected: number = Math.trunc((n * ((n + 1))) / 2)
	return (expected - sum)
}

function example_1(): void {
	if (!((missingNumber([3, 0, 1]) == 2))) { throw new Error('expect failed') }
}

function example_2(): void {
	if (!((missingNumber([0, 1]) == 2))) { throw new Error('expect failed') }
}

function example_3(): void {
	if (!((missingNumber([9, 6, 4, 2, 3, 5, 7, 0, 1]) == 8))) { throw new Error('expect failed') }
}

function single_zero(): void {
	if (!((missingNumber([0]) == 1))) { throw new Error('expect failed') }
}

function already_ordered(): void {
	if (!((missingNumber([0, 2, 1, 4, 5, 6, 7, 8, 9]) == 3))) { throw new Error('expect failed') }
}

function main(): void {
	example_1()
	example_2()
	example_3()
	single_zero()
	already_ordered()
}
main()

