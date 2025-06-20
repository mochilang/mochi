// Generated by Mochi TypeScript compiler

function canWin(s: string) : boolean {
	let memo: Record<string, boolean> = {}
	function helper(cur: string) : boolean {
		if (Object.prototype.hasOwnProperty.call(memo, String(cur))) {
			return memo[cur]
		}
		let i: number = 0
		while (((i + 1) < cur.length)) {
			if (((cur[i] == "+") && (cur[(i + 1)] == "+"))) {
				let next: string = cur.slice(0, i) + "--" + cur.slice((i + 2), cur.length)
				if ((!helper(next))) {
					memo[cur] = true
					return true
				}
			}
			i = (i + 1)
		}
		memo[cur] = false
		return false
	}
	return helper(s)
}

function example_1(): void {
	if (!((canWin("++++") == true))) { throw new Error('expect failed') }
}

function example_2(): void {
	if (!((canWin("+") == false))) { throw new Error('expect failed') }
}

function five_plus(): void {
	if (!((canWin("+++++") == false))) { throw new Error('expect failed') }
}

function mixed(): void {
	if (!((canWin("+-++") == true))) { throw new Error('expect failed') }
}

function main(): void {
	example_1()
	example_2()
	five_plus()
	mixed()
}
main()

