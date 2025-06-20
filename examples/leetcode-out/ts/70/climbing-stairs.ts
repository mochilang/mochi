// Generated by Mochi TypeScript compiler

function climbStairs(n: number) : number {
	if ((n <= 2)) {
		return n
	}
	let first: number = 1
	let second: number = 2
	let i: number = 3
	while ((i <= n)) {
		let next: number = (first + second)
		first = second
		second = next
		i = (i + 1)
	}
	return second
}

function example_1(): void {
	if (!((climbStairs(2) == 2))) { throw new Error('expect failed') }
}

function example_2(): void {
	if (!((climbStairs(3) == 3))) { throw new Error('expect failed') }
}

function n___4(): void {
	if (!((climbStairs(4) == 5))) { throw new Error('expect failed') }
}

function n___5(): void {
	if (!((climbStairs(5) == 8))) { throw new Error('expect failed') }
}

function main(): void {
	example_1()
	example_2()
	n___4()
	n___5()
}
main()

