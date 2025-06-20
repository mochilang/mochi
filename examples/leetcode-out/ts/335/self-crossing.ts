// Generated by Mochi TypeScript compiler

function isSelfCrossing(distance: Array<number>) : boolean {
	let n: number = distance.length
	if ((n < 4)) {
		return false
	}
	let i: number = 3
	while ((i < n)) {
		if (((distance[i] >= distance[(i - 2)]) && (distance[(i - 1)] <= distance[(i - 3)]))) {
			return true
		}
		if ((((i >= 4) && (distance[(i - 1)] == distance[(i - 3)])) && ((distance[i] + distance[(i - 4)]) >= distance[(i - 2)]))) {
			return true
		}
		if ((((((i >= 5) && (distance[(i - 2)] >= distance[(i - 4)])) && (distance[(i - 1)] <= distance[(i - 3)])) && ((distance[i] + distance[(i - 4)]) >= distance[(i - 2)])) && ((distance[(i - 1)] + distance[(i - 5)]) >= distance[(i - 3)]))) {
			return true
		}
		i = (i + 1)
	}
	return false
}

function example_1(): void {
	if (!((isSelfCrossing([2, 1, 1, 2]) == true))) { throw new Error('expect failed') }
}

function example_2(): void {
	if (!((isSelfCrossing([1, 2, 3, 4]) == false))) { throw new Error('expect failed') }
}

function example_3(): void {
	if (!((isSelfCrossing([1, 1, 1, 2, 1]) == true))) { throw new Error('expect failed') }
}

function short(): void {
	if (!((isSelfCrossing([1, 2, 1]) == false))) { throw new Error('expect failed') }
}

function no_crossing(): void {
	if (!((isSelfCrossing([3, 3, 4, 2, 2]) == false))) { throw new Error('expect failed') }
}

function cross_late(): void {
	if (!((isSelfCrossing([1, 1, 2, 1, 1]) == true))) { throw new Error('expect failed') }
}

function main(): void {
	example_1()
	example_2()
	example_3()
	short()
	no_crossing()
	cross_late()
}
main()

