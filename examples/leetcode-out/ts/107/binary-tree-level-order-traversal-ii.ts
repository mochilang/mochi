// Generated by Mochi TypeScript compiler

type Leaf = {
  __name: "Leaf";
};

type _Node = {
  __name: "Node";
  left: Tree;
  value: number;
  right: Tree;
};

type Tree = Leaf | _Node;

function levelOrderBottom(root: Tree): Array<Array<number>> {
  if (
    (() => {
      const _t = root;
      if (_t.__name === "Leaf") return true;
      return false;
    })()
  ) {
    return [];
  }
  let result: Array<Array<number>> = [];
  (globalThis as any).result = result;
  let queue: Array<Tree> = [root];
  (globalThis as any).queue = queue;
  while ((queue.length > 0)) {
    let level: Array<number> = [];
    (globalThis as any).level = level;
    let next: Array<Tree> = [];
    (globalThis as any).next = next;
    for (const node of queue) {
      if (
        (() => {
          const _t = node;
          if (_t.__name === "Leaf") return false;
          return true;
        })()
      ) {
        level = level.concat([node.value]);
        if (
          (() => {
            const _t = node.left;
            if (_t.__name === "Leaf") return false;
            return true;
          })()
        ) {
          next = next.concat([node.left]);
        }
        if (
          (() => {
            const _t = node.right;
            if (_t.__name === "Leaf") return false;
            return true;
          })()
        ) {
          next = next.concat([node.right]);
        }
      }
    }
    result = [level].concat(result);
    queue = next;
  }
  return result;
}

function test_example_1(): void {
  if (
    !(_equal(levelOrderBottom(example1), [
      [
        15,
        7,
      ],
      [
        9,
        20,
      ],
      [3],
    ]))
  ) throw new Error("expect failed");
}

function test_single_node(): void {
  if (
    !(_equal(
      levelOrderBottom({
        __name: "Node",
        left: { __name: "Leaf" },
        value: 1,
        right: { __name: "Leaf" },
      }),
      [[1]],
    ))
  ) throw new Error("expect failed");
}

function test_empty(): void {
  if (!(_equal(levelOrderBottom({ __name: "Leaf" }), []))) {
    throw new Error("expect failed");
  }
}

function main(): void {
  let example1: _Node = {
    __name: "Node",
    left: {
      __name: "Node",
      left: { __name: "Leaf" },
      value: 9,
      right: { __name: "Leaf" },
    },
    value: 3,
    right: {
      __name: "Node",
      left: {
        __name: "Node",
        left: { __name: "Leaf" },
        value: 15,
        right: { __name: "Leaf" },
      },
      value: 20,
      right: {
        __name: "Node",
        left: { __name: "Leaf" },
        value: 7,
        right: { __name: "Leaf" },
      },
    },
  };
  (globalThis as any).example1 = example1;
  test_example_1();
  test_single_node();
  test_empty();
}
function _equal(a: any, b: any): boolean {
  if (Array.isArray(a) && Array.isArray(b)) {
    if (a.length !== b.length) return false;
    for (let i = 0; i < a.length; i++) if (!_equal(a[i], b[i])) return false;
    return true;
  }
  if (a && b && typeof a === "object" && typeof b === "object") {
    const ak = Object.keys(a);
    const bk = Object.keys(b);
    if (ak.length !== bk.length) return false;
    for (const k of ak) {
      if (!bk.includes(k) || !_equal((a as any)[k], (b as any)[k])) {
        return false;
      }
    }
    return true;
  }
  return a === b;
}

main();
