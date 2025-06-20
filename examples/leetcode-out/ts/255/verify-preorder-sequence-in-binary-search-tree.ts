// Generated by Mochi TypeScript compiler

function verifyPreorder(preorder: Array<number>) : boolean {
	let stack: Array<number> = []
	let lower: number = (-2147483648)
	for (const value of preorder) {
		if ((value < lower)) {
			return false
		}
		while ((stack.length > 0)) {
			let top: number = stack[(stack.length - 1)]
			if ((value > top)) {
				lower = top
				stack = stack.slice(0, (stack.length - 1))
			} else {
				break
			}
		}
		stack = stack.concat([value])
	}
	return true
}

function example_1(): void {
	if (!((verifyPreorder([5, 2, 1, 3, 6]) == true))) { throw new Error('expect failed') }
}

function example_2(): void {
	if (!((verifyPreorder([5, 2, 6, 1, 3]) == false))) { throw new Error('expect failed') }
}

function single_node(): void {
	if (!((verifyPreorder([1]) == true))) { throw new Error('expect failed') }
}

function empty_list(): void {
	if (!((verifyPreorder([]) == true))) { throw new Error('expect failed') }
}

function strictly_increasing(): void {
	if (!((verifyPreorder([1, 2, 3, 4, 5]) == true))) { throw new Error('expect failed') }
}

function main(): void {
	example_1()
	example_2()
	single_node()
	empty_list()
	strictly_increasing()
}
main()

