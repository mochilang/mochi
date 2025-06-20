// Generated by Mochi TypeScript compiler

function maxArea(height: Array<number>) : number {
	let left: number = 0
	let right: number = (height.length - 1)
	let maxArea: number = 0
	while ((left < right)) {
		let width: number = (right - left)
		let h: number = 0
		if ((height[left] < height[right])) {
			h = height[left]
		} else {
			h = height[right]
		}
		let area: number = (h * width)
		if ((area > maxArea)) {
			maxArea = area
		}
		if ((height[left] < height[right])) {
			left = (left + 1)
		} else {
			right = (right - 1)
		}
	}
	return maxArea
}

function example_1(): void {
	if (!((maxArea([1, 8, 6, 2, 5, 4, 8, 3, 7]) == 49))) { throw new Error('expect failed') }
}

function example_2(): void {
	if (!((maxArea([1, 1]) == 1))) { throw new Error('expect failed') }
}

function decreasing_heights(): void {
	if (!((maxArea([4, 3, 2, 1, 4]) == 16))) { throw new Error('expect failed') }
}

function short_array(): void {
	if (!((maxArea([1, 2, 1]) == 2))) { throw new Error('expect failed') }
}

function main(): void {
	example_1()
	example_2()
	decreasing_heights()
	short_array()
}
main()

