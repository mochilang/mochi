// Generated by Mochi TypeScript compiler

function numDistinct(s: string, t: string) : number {
	let m: number = s.length
	let n: number = t.length
	let dp: Array<Array<number>> = []
	let i: number = 0
	while ((i <= m)) {
		let row: Array<number> = []
		let j: number = 0
		while ((j <= n)) {
			row = row.concat([0])
			j = (j + 1)
		}
		dp = dp.concat([row])
		i = (i + 1)
	}
	i = 0
	while ((i <= m)) {
		dp[i][0] = 1
		i = (i + 1)
	}
	i = 1
	while ((i <= m)) {
		let j: number = 1
		while ((j <= n)) {
			dp[i][j] = dp[(i - 1)][j]
			if ((s[(i - 1)] == t[(j - 1)])) {
				dp[i][j] = (dp[i][j] + dp[(i - 1)][(j - 1)])
			}
			j = (j + 1)
		}
		i = (i + 1)
	}
	return dp[m][n]
}

function example_1(): void {
	if (!((numDistinct("rabbbit", "rabbit") == 3))) { throw new Error('expect failed') }
}

function example_2(): void {
	if (!((numDistinct("babgbag", "bag") == 5))) { throw new Error('expect failed') }
}

function empty_target(): void {
	if (!((numDistinct("abc", "") == 1))) { throw new Error('expect failed') }
}

function no_subsequence(): void {
	if (!((numDistinct("abc", "abcd") == 0))) { throw new Error('expect failed') }
}

function main(): void {
	example_1()
	example_2()
	empty_target()
	no_subsequence()
}
main()

