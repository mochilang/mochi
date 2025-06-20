// Generated by Mochi TypeScript compiler

function combinationSum4(nums: Array<number>, target: number) : number {
	let dp: Array<number> = [1]
	let t: number = 1
	while ((t <= target)) {
		let count: number = 0
		for (const num of nums) {
			if ((num <= t)) {
				count = (count + dp[(t - num)])
			}
		}
		dp = dp.concat([count])
		t = (t + 1)
	}
	return dp[target]
}

function example_1(): void {
	if (!((combinationSum4([1, 2, 3], 4) == 7))) { throw new Error('expect failed') }
}

function example_2(): void {
	if (!((combinationSum4([9], 3) == 0))) { throw new Error('expect failed') }
}

function main(): void {
	example_1()
	example_2()
}
main()

