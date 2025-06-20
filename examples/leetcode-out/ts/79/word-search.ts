// Generated by Mochi TypeScript compiler

let board: Array<Array<string>> = [["A", "B", "C", "E"], ["S", "F", "C", "S"], ["A", "D", "E", "E"]]

function exist(board: Array<Array<string>>, word: string) : boolean {
	let m: number = board.length
	if ((m == 0)) {
		return false
	}
	let n: number = board[0].length
	let visited: Array<Array<boolean>> = []
	let r: number = 0
	while ((r < m)) {
		let row: Array<boolean> = []
		let c: number = 0
		while ((c < n)) {
			row = row.concat([false])
			c = (c + 1)
		}
		visited = visited.concat([row])
		r = (r + 1)
	}
	function dfs(r: number, c: number, idx: number) : boolean {
		if ((idx == word.length)) {
			return true
		}
		if (((((r < 0) || (r >= m)) || (c < 0)) || (c >= n))) {
			return false
		}
		if (visited[r][c]) {
			return false
		}
		if ((board[r][c] != word[idx])) {
			return false
		}
		visited[r][c] = true
		if ((((dfs((r + 1), c, (idx + 1)) || dfs((r - 1), c, (idx + 1))) || dfs(r, (c + 1), (idx + 1))) || dfs(r, (c - 1), (idx + 1)))) {
			visited[r][c] = false
			return true
		}
		visited[r][c] = false
		return false
	}
	for (let i: number = 0; i < m; i++) {
		for (let j: number = 0; j < n; j++) {
			if (dfs(i, j, 0)) {
				return true
			}
		}
	}
	return false
}

function example_1(): void {
	if (!((exist(board, "ABCCED") == true))) { throw new Error('expect failed') }
}

function example_2(): void {
	if (!((exist(board, "SEE") == true))) { throw new Error('expect failed') }
}

function example_3(): void {
	if (!((exist(board, "ABCB") == false))) { throw new Error('expect failed') }
}

function main(): void {
	example_1()
	example_2()
	example_3()
}
main()

