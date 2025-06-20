// Generated by Mochi TypeScript compiler

let ex: any = {__name: "Node", left: {__name: "Node", left: {__name: "Leaf"}, value: 3, right: {__name: "Leaf"}}, value: 1, right: {__name: "Node", left: {__name: "Leaf"}, value: 4, right: {__name: "Leaf"}}}

let fixed: Tree = recoverTree(ex)

let ex2: any = {__name: "Node", left: {__name: "Node", left: {__name: "Leaf"}, value: 2, right: {__name: "Leaf"}}, value: 4, right: {__name: "Node", left: {__name: "Leaf"}, value: 1, right: {__name: "Leaf"}}}

let fixed2: Tree = recoverTree(ex2)

function inorder(t: Tree) : Array<number> {
	return (() => {
	const _t = t;
	if (_t.__name === "Leaf") { return [] }
	if (_t.__name === "Node") { return ((l, v, r) => inorder(l).concat([v]).concat(inorder(r)))(_t.left, _t.value, _t.right) }
	return undefined
})()
}

function recoverTree(t: Tree) : Tree {
	let vals: Array<number> = inorder(t)
	let sortedVals: Array<any> = (() => {
	const _src = vals;
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
	function build(lo: number, hi: number) : any {
		if ((lo >= hi)) {
			return {__name: "Leaf"}
		}
		let mid: number = Math.trunc(((lo + hi)) / 2)
		return {__name: "Node", left: build(lo, mid), value: sortedVals[mid], right: build((mid + 1), hi)}
	}
	return build(0, sortedVals.length)
}

function main(): void {
	type Leaf = {
		__name: "Leaf";
	}
	
	type Node = {
		__name: "Node";
		left: any;
		value: number;
		right: any;
	}
	
	type Tree = Leaf | Node
	if (!(_equal(inorder(fixed), [1, 3, 4]))) { throw new Error('expect failed') }
	if (!(_equal(inorder(fixed2), [1, 2, 4]))) { throw new Error('expect failed') }
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

