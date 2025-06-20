// Generated by Mochi TypeScript compiler

let example: Array<Array<string>> = [["0", "0", "1", "0"], ["0", "1", "1", "0"], ["0", "1", "0", "0"]]

function minArea(image: Array<Array<string>>, x: number, y: number) : number {
	let rows: number = image.length
	let cols: number = image[0].length
	let visited: Array<Array<boolean>> = []
	let r: number = 0
	while ((r < rows)) {
		let row: Array<boolean> = []
		let c: number = 0
		while ((c < cols)) {
			row = row.concat([false])
			c = (c + 1)
		}
		visited = visited.concat([row])
		r = (r + 1)
	}
	let minRow: number = x
	let maxRow: number = x
	let minCol: number = y
	let maxCol: number = y
	function dfs(i: number, j: number) : number {
		if (((((i < 0) || (i >= rows)) || (j < 0)) || (j >= cols))) {
			return 0
		}
		if (visited[i][j]) {
			return 0
		}
		if ((image[i][j] != "1")) {
			return 0
		}
		visited[i][j] = true
		if ((i < minRow)) {
			minRow = i
		}
		if ((i > maxRow)) {
			maxRow = i
		}
		if ((j < minCol)) {
			minCol = j
		}
		if ((j > maxCol)) {
			maxCol = j
		}
		dfs((i + 1), j)
		dfs((i - 1), j)
		dfs(i, (j + 1))
		dfs(i, (j - 1))
		return 0
	}
	dfs(x, y)
	let height: number = ((maxRow - minRow) + 1)
	let width: number = ((maxCol - minCol) + 1)
	return (height * width)
}

function example(): void {
	if (!((minArea(example, 0, 2) == 6))) { throw new Error('expect failed') }
}

function single_pixel(): void {
	if (!((minArea([["1"]], 0, 0) == 1))) { throw new Error('expect failed') }
}

function all_ones(): void {
	if (!((minArea([["1", "1"], ["1", "1"]], 1, 1) == 4))) { throw new Error('expect failed') }
}

function main(): void {
	example()
	single_pixel()
	all_ones()
}
main()

