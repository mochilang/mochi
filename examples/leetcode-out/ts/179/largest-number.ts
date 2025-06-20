// Generated by Mochi TypeScript compiler

function largestNumber(nums: Array<number>) : string {
	let strs: Array<string> = []
	let i: number = 0
	while ((i < nums.length)) {
		strs = strs.concat([String(nums[i])])
		i = (i + 1)
	}
	let j: number = 0
	while ((j < strs.length)) {
		let k: number = (j + 1)
		while ((k < strs.length)) {
			let ab: string = strs[j] + strs[k]
			let ba: string = strs[k] + strs[j]
			if ((ab < ba)) {
				let tmp: string = strs[j]
				strs[j] = strs[k]
				strs[k] = tmp
			}
			k = (k + 1)
		}
		j = (j + 1)
	}
	let result: string = ""
	i = 0
	while ((i < strs.length)) {
		result = result + strs[i]
		i = (i + 1)
	}
	let pos: number = 0
	while (((pos < (result.length - 1)) && (result[pos] == "0"))) {
		pos = (pos + 1)
	}
	return result.slice(pos, result.length)
}

function example_1(): void {
	if (!((largestNumber([10, 2]) == "210"))) { throw new Error('expect failed') }
}

function example_2(): void {
	if (!((largestNumber([3, 30, 34, 5, 9]) == "9534330"))) { throw new Error('expect failed') }
}

function multiple_zeros(): void {
	if (!((largestNumber([0, 0]) == "0"))) { throw new Error('expect failed') }
}

function main(): void {
	example_1()
	example_2()
	multiple_zeros()
}
main()

