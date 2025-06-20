// Generated by Mochi TypeScript compiler

function newCounter() : HitCounter {
	return {data: []}
}

function clean(c: HitCounter, timestamp: number) : HitCounter {
	let d: Array<Entry> = c.data
	let i: number = 0
	while ((i < d.length)) {
		let ent: Entry = d[i]
		if (((timestamp - ent.ts) >= 300)) {
			i = (i + 1)
		} else {
			break
		}
	}
	d = d.slice(i, d.length)
	return {data: d}
}

function hit(c: HitCounter, timestamp: number) : HitCounter {
	let counter: HitCounter = clean(c, timestamp)
	let d: Array<Entry> = counter.data
	if ((d.length > 0)) {
		let last: Entry = d[(d.length - 1)]
		if ((last.ts == timestamp)) {
			d = d.slice(0, (d.length - 1)).concat([{ts: last.ts, cnt: (last.cnt + 1)}])
		} else {
			d = d.concat([{ts: timestamp, cnt: 1}])
		}
	} else {
		d = d.concat([{ts: timestamp, cnt: 1}])
	}
	return {data: d}
}

function getHits(c: HitCounter, timestamp: number) : number {
	let counter: HitCounter = clean(c, timestamp)
	let sum: number = 0
	for (const e of counter.data) {
		sum = (sum + e.cnt)
	}
	return sum
}

function example(): void {
	let c: HitCounter = newCounter()
	c = hit(c, 1)
	c = hit(c, 2)
	c = hit(c, 3)
	if (!((getHits(c, 4) == 3))) { throw new Error('expect failed') }
	c = hit(c, 300)
	if (!((getHits(c, 300) == 4))) { throw new Error('expect failed') }
	if (!((getHits(c, 301) == 3))) { throw new Error('expect failed') }
}

function expire(): void {
	let c: HitCounter = newCounter()
	c = hit(c, 1)
	c = hit(c, 10)
	c = hit(c, 300)
	if (!((getHits(c, 300) == 3))) { throw new Error('expect failed') }
	c = hit(c, 601)
	if (!((getHits(c, 601) == 1))) { throw new Error('expect failed') }
}

function main(): void {
	type Entry = {
		ts: number;
		cnt: number;
	}
	type HitCounter = {
		data: Array<any>;
	}
	example()
	expire()
}
main()

