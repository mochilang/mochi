package main

func expect(cond bool) {
	if !cond { panic("expect failed") }
}

func numberOfPatterns(m int, n int) int {
	var jump [][]int = [][]int{}
	var i int = 0
	for (i < 10) {
		var row []int = []int{}
		var j int = 0
		for (j < 10) {
			row = append(append([]int{}, row...), []int{0}...)
			j = (j + 1)
		}
		jump = append(append([][]int{}, jump...), [][]int{row}...)
		i = (i + 1)
	}
	jump[1][3] = 2
	jump[3][1] = 2
	jump[1][7] = 4
	jump[7][1] = 4
	jump[3][9] = 6
	jump[9][3] = 6
	jump[7][9] = 8
	jump[9][7] = 8
	jump[1][9] = 5
	jump[9][1] = 5
	jump[3][7] = 5
	jump[7][3] = 5
	jump[4][6] = 5
	jump[6][4] = 5
	jump[2][8] = 5
	jump[8][2] = 5
	var visited []bool = []bool{}
	i = 0
	for (i < 10) {
		visited = append(append([]bool{}, visited...), []bool{false}...)
		i = (i + 1)
	}
	var dfs func(int, int) int
	dfs = func(num int, remain int) int {
		if (remain == 0) {
			return 1
		}
		visited[num] = true
		var count int = 0
		for next := 1; next < 10; next++ {
			if !visited[next] {
				var mid int = jump[num][next]
				if ((mid == 0) || visited[mid]) {
					count = (count + dfs(next, (remain - 1)))
				}
			}
		}
		visited[num] = false
		return count
}
	var total int = 0
	var len int = m
	for (len <= n) {
		for start := 1; start < 10; start++ {
			total = (total + dfs(start, (len - 1)))
		}
		len = (len + 1)
	}
	return total
}

func example_1() {
	expect((numberOfPatterns(1, 1) == 9))
}

func example_2() {
	expect((numberOfPatterns(1, 2) == 65))
}

func example_3() {
	expect((numberOfPatterns(2, 2) == 56))
}

func main() {
	example_1()
	example_2()
	example_3()
}

