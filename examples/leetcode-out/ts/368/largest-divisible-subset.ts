// Generated by Mochi TypeScript compiler

function largestDivisibleSubset(nums: Array<number>) : Array<number> {
	let n: number = nums.length
	if ((n == 0)) {
		return []
	}
	let sorted: Array<any> = (() => {
	const _src = nums;
	let _items = [];
	for (const x of _src) {
		_items.push(x);
	}
	let _pairs = _items.map(it => { const x = it; return {item: it, key: x}; });
	_pairs.sort((a, b) => {
		const ak = a.key; const bk = b.key;
		if (typeof ak === 'number' && typeof bk === 'number') return ak - bk;
		if (typeof ak === 'string' && typeof bk === 'string') return ak < bk ? -1 : (ak > bk ? 1 : 0);
		return String(ak) < String(bk) ? -1 : (String(ak) > String(bk) ? 1 : 0);
	});
	_items = _pairs.map(p => p.item);
	const _res = [];
	for (const x of _items) {
		_res.push(x)
	}
	return _res;
})()
	let dp: Array<number> = []
	let parent: Array<number> = []
	let fill: number = 0
	while ((fill < n)) {
		dp = dp.concat([1])
		parent = parent.concat([(-1)])
		fill = (fill + 1)
	}
	let maxLen: number = 1
	let maxIdx: number = 0
	let i: number = 0
	while ((i < n)) {
		let j: number = 0
		while ((j < i)) {
			if (_equal((sorted[i] % sorted[j]), 0)) {
				let candidate: number = (dp[j] + 1)
				if ((candidate > dp[i])) {
					dp[i] = candidate
					parent[i] = j
				}
			}
			j = (j + 1)
		}
		if ((dp[i] > maxLen)) {
			maxLen = dp[i]
			maxIdx = i
		}
		i = (i + 1)
	}
	let subset: Array<number> = []
	let k: number = maxIdx
	while ((k >= 0)) {
		subset = [sorted[k]].concat(subset)
		k = parent[k]
	}
	return subset
}

function example_1(): void {
	if (!(_equal(largestDivisibleSubset([1, 2, 3]), [1, 2]))) { throw new Error('expect failed') }
}

function example_2(): void {
	if (!(_equal(largestDivisibleSubset([1, 2, 4, 8]), [1, 2, 4, 8]))) { throw new Error('expect failed') }
}

function empty(): void {
	if (!(_equal(largestDivisibleSubset([]), []))) { throw new Error('expect failed') }
}

function mixed_numbers(): void {
	if (!(_equal(largestDivisibleSubset([4, 8, 10, 240]), [4, 8, 240]))) { throw new Error('expect failed') }
}

function main(): void {
	example_1()
	example_2()
	empty()
	mixed_numbers()
}
function _equal(a: any, b: any): boolean {
  if (Array.isArray(a) && Array.isArray(b)) {
    if (a.length !== b.length) return false;
    for (let i = 0; i < a.length; i++) { if (!_equal(a[i], b[i])) return false; }
    return true;
  }
  if (a && b && typeof a === 'object' && typeof b === 'object') {
    const ak = Object.keys(a); const bk = Object.keys(b);
    if (ak.length !== bk.length) return false;
    for (const k of ak) { if (!bk.includes(k) || !_equal((a as any)[k], (b as any)[k])) return false; }
    return true;
  }
  return a === b;
}

main()

