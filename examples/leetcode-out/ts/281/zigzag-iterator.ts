// Generated by Mochi TypeScript compiler

function newZigzagIterator(v1: Array<number>, v2: Array<number>) : ZigzagIterator {
	return {v1: v1, v2: v2, i1: 0, i2: 0, turn: 0}
}

function zigzagHasNext(it: ZigzagIterator) : boolean {
	return ((it.i1 < it.v1.length) || (it.i2 < it.v2.length))
}

function zigzagNext(it: ZigzagIterator) : NextResult {
	let idx1: number = it.i1
	let idx2: number = it.i2
	let t: number = it.turn
	let a: Array<number> = it.v1
	let b: Array<number> = it.v2
	let value: number = 0
	if ((t == 0)) {
		if ((idx1 < a.length)) {
			value = a[idx1]
			idx1 = (idx1 + 1)
			if ((idx2 < b.length)) {
				t = 1
			}
		} else {
			value = b[idx2]
			idx2 = (idx2 + 1)
		}
	} else {
		if ((idx2 < b.length)) {
			value = b[idx2]
			idx2 = (idx2 + 1)
			if ((idx1 < a.length)) {
				t = 0
			}
		} else {
			value = a[idx1]
			idx1 = (idx1 + 1)
		}
	}
	return {it: {v1: a, v2: b, i1: idx1, i2: idx2, turn: t}, val: value}
}

function example(): void {
	let it: ZigzagIterator = newZigzagIterator([1, 2], [3, 4, 5, 6])
	if (!((zigzagHasNext(it) == true))) { throw new Error('expect failed') }
	let r1: NextResult = zigzagNext(it)
	it = r1.it
	if (!((r1.val == 1))) { throw new Error('expect failed') }
	if (!((zigzagHasNext(it) == true))) { throw new Error('expect failed') }
	let r2: NextResult = zigzagNext(it)
	it = r2.it
	if (!((r2.val == 3))) { throw new Error('expect failed') }
	if (!((zigzagHasNext(it) == true))) { throw new Error('expect failed') }
	let r3: NextResult = zigzagNext(it)
	it = r3.it
	if (!((r3.val == 2))) { throw new Error('expect failed') }
	if (!((zigzagHasNext(it) == true))) { throw new Error('expect failed') }
	let r4: NextResult = zigzagNext(it)
	it = r4.it
	if (!((r4.val == 4))) { throw new Error('expect failed') }
	if (!((zigzagHasNext(it) == true))) { throw new Error('expect failed') }
	let r5: NextResult = zigzagNext(it)
	it = r5.it
	if (!((r5.val == 5))) { throw new Error('expect failed') }
	if (!((zigzagHasNext(it) == true))) { throw new Error('expect failed') }
	let r6: NextResult = zigzagNext(it)
	it = r6.it
	if (!((r6.val == 6))) { throw new Error('expect failed') }
	if (!((zigzagHasNext(it) == false))) { throw new Error('expect failed') }
}

function uneven_lists(): void {
	let it: ZigzagIterator = newZigzagIterator([1, 2, 3], [4])
	let r1: NextResult = zigzagNext(it)
	it = r1.it
	if (!((r1.val == 1))) { throw new Error('expect failed') }
	let r2: NextResult = zigzagNext(it)
	it = r2.it
	if (!((r2.val == 4))) { throw new Error('expect failed') }
	let r3: NextResult = zigzagNext(it)
	it = r3.it
	if (!((r3.val == 2))) { throw new Error('expect failed') }
	let r4: NextResult = zigzagNext(it)
	it = r4.it
	if (!((r4.val == 3))) { throw new Error('expect failed') }
	if (!((zigzagHasNext(it) == false))) { throw new Error('expect failed') }
}

function empty_second_list(): void {
	let it: ZigzagIterator = newZigzagIterator([7, 8], [])
	let r1: NextResult = zigzagNext(it)
	it = r1.it
	if (!((r1.val == 7))) { throw new Error('expect failed') }
	let r2: NextResult = zigzagNext(it)
	it = r2.it
	if (!((r2.val == 8))) { throw new Error('expect failed') }
	if (!((zigzagHasNext(it) == false))) { throw new Error('expect failed') }
}

function main(): void {
	type ZigzagIterator = {
		v1: Array<number>;
		v2: Array<number>;
		i1: number;
		i2: number;
		turn: number;
	}
	type NextResult = {
		it: any;
		val: number;
	}
	example()
	uneven_lists()
	empty_second_list()
}
main()

