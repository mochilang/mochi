// Generated by Mochi TypeScript compiler

function newMovingAverage(size: number) : MovingAverage {
	return {size: size, nums: [], total: 0}
}

function next(ma: MovingAverage, val: number) : NextResult {
	let nums: Array<number> = ma.nums
	let total: number = ma.total
	if ((nums.length == ma.size)) {
		total = (total - nums[0])
		nums = nums.slice(1, nums.length)
	}
	nums = nums.concat([val])
	total = (total + val)
	let updated: MovingAverage = {size: ma.size, nums: nums, total: total}
	let avg: number = ((total) / (nums.length))
	return {ma: updated, average: avg}
}

function example(): void {
	let ma: MovingAverage = newMovingAverage(3)
	let r1: NextResult = next(ma, 1)
	ma = r1.ma
	if (!((r1.average == 1))) { throw new Error('expect failed') }
	let r2: NextResult = next(ma, 10)
	ma = r2.ma
	if (!((r2.average == 5.5))) { throw new Error('expect failed') }
	let r3: NextResult = next(ma, 3)
	ma = r3.ma
	if (!((r3.average == (14 / 3)))) { throw new Error('expect failed') }
	let r4: NextResult = next(ma, 5)
	ma = r4.ma
	if (!((r4.average == 6))) { throw new Error('expect failed') }
}

function single_element_window(): void {
	let ma: MovingAverage = newMovingAverage(1)
	let r: NextResult = next(ma, 4)
	ma = r.ma
	if (!((r.average == 4))) { throw new Error('expect failed') }
	let r2: NextResult = next(ma, 7)
	ma = r2.ma
	if (!((r2.average == 7))) { throw new Error('expect failed') }
}

function window_smaller_than_inputs(): void {
	let ma: MovingAverage = newMovingAverage(2)
	let a: NextResult = next(ma, 3)
	ma = a.ma
	if (!((a.average == 3))) { throw new Error('expect failed') }
	let b: NextResult = next(ma, 5)
	ma = b.ma
	if (!((b.average == 4))) { throw new Error('expect failed') }
	let c: NextResult = next(ma, 7)
	ma = c.ma
	if (!((c.average == 6))) { throw new Error('expect failed') }
}

function main(): void {
	type MovingAverage = {
		size: number;
		nums: Array<number>;
		total: number;
	}
	type NextResult = {
		ma: any;
		average: number;
	}
	example()
	single_element_window()
	window_smaller_than_inputs()
}
main()

