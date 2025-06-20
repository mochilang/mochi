// Generated by Mochi TypeScript compiler

let example1: Record<string, any> = Node(Node(Leaf(), 2, Node(Leaf(), 5, Leaf())), 1, Node(Leaf(), 3, Node(Leaf(), 4, Leaf())))

function Leaf() : Record<string, any> {
	return {"__name": "Leaf"}
}

function Node(left: Record<string, any>, value: number, right: Record<string, any>) : Record<string, any> {
	return {"__name": "Node", "left": left, "value": value, "right": right}
}

function isLeaf(t: Record<string, any>) : boolean {
	return _equal(t["__name"], "Leaf")
}

function left(t: Record<string, any>) : Record<string, any> {
	return t["left"]
}

function right(t: Record<string, any>) : Record<string, any> {
	return t["right"]
}

function value(t: Record<string, any>) : number {
	return t["value"]
}

function rightSideView(root: Record<string, any>) : Array<number> {
	let result: Array<number> = []
	let queue: Array<Record<string, any>> = []
	if ((!isLeaf(root))) {
		queue = [root]
	}
	while ((queue.length > 0)) {
		let next: Array<Record<string, any>> = []
		let last: number = 0
		for (const node of queue) {
			last = value(node)
			let l: Record<string, any> = left(node)
			let r: Record<string, any> = right(node)
			if ((!isLeaf(l))) {
				next = next.concat([l])
			}
			if ((!isLeaf(r))) {
				next = next.concat([r])
			}
		}
		result = result.concat([last])
		queue = next
	}
	return result
}

function example_1(): void {
	if (!(_equal(rightSideView(example1), [1, 3, 4]))) { throw new Error('expect failed') }
}

function example_2(): void {
	if (!(_equal(rightSideView(Node(Leaf(), 1, Node(Leaf(), 3, Leaf()))), [1, 3]))) { throw new Error('expect failed') }
}

function empty(): void {
	if (!(_equal(rightSideView(Leaf()), []))) { throw new Error('expect failed') }
}

function main(): void {
	example_1()
	example_2()
	empty()
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

