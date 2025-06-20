// Generated by Mochi TypeScript compiler

function reverseBits(n: number) : number {
	let result: number = 0
	let x: number = n
	let count: number = 0
	while ((count < 32)) {
		let bit: number = (x % 2)
		result = ((result * 2) + bit)
		x = Math.trunc(x / 2)
		count = (count + 1)
	}
	return result
}

function example_1(): void {
	if (!((reverseBits(43261596) == 964176192))) { throw new Error('expect failed') }
}

function example_2(): void {
	if (!((reverseBits(4294967293) == 3221225471))) { throw new Error('expect failed') }
}

function main(): void {
	example_1()
	example_2()
}
main()

