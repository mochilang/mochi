// Generated by Mochi TypeScript compiler

function longestConsecutive(nums: Array<number>) : number {
	let set: Record<number, boolean> = {}
	for (const n of nums) {
		set[n] = true
	}
	let best: number = 0
	for (const nKey of Object.keys(set)) {
		const n: number = Number(nKey)
		if ((!(Object.prototype.hasOwnProperty.call(set, String((n - 1)))))) {
			let curr: number = n
			let length: number = 1
			while (Object.prototype.hasOwnProperty.call(set, String((curr + 1)))) {
				curr = (curr + 1)
				length = (length + 1)
			}
			if ((length > best)) {
				best = length
			}
		}
	}
	return best
}

function example_1(): void {
	if (!((longestConsecutive([100, 4, 200, 1, 3, 2]) == 4))) { throw new Error('expect failed') }
}

function example_2(): void {
	if (!((longestConsecutive([0, 3, 7, 2, 5, 8, 4, 6, 0, 1]) == 9))) { throw new Error('expect failed') }
}

function empty(): void {
	if (!((longestConsecutive([]) == 0))) { throw new Error('expect failed') }
}

function duplicates(): void {
	if (!((longestConsecutive([1, 2, 0, 1]) == 3))) { throw new Error('expect failed') }
}

function main(): void {
	example_1()
	example_2()
	empty()
	duplicates()
}
main()

