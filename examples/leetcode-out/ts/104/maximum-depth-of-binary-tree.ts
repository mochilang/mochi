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

function max(a: number, b: number): number {
  if ((a > b)) {
    return a;
  } else {
    return b;
  }
}

function maxDepth(root: Tree): number {
  return (() => {
    const _t = root;
    if (_t.__name === "Leaf") return 0;
    if (_t.__name === "Node") {
      return ((l, r) => (_max(maxDepth(l), maxDepth(r)) + 1))(
        _t.left,
        _t.right,
      );
    }
    return undefined;
  })();
}

function test_example_1(): void {
  let tree: _Node = {
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
  (globalThis as any).tree = tree;
  if (!(maxDepth(tree) == 3)) throw new Error("expect failed");
}

function test_example_2(): void {
  let tree: _Node = {
    __name: "Node",
    left: { __name: "Leaf" },
    value: 1,
    right: {
      __name: "Node",
      left: { __name: "Leaf" },
      value: 2,
      right: { __name: "Leaf" },
    },
  };
  (globalThis as any).tree = tree;
  if (!(maxDepth(tree) == 2)) throw new Error("expect failed");
}

function test_single_node(): void {
  if (
    !(maxDepth({
      __name: "Node",
      left: { __name: "Leaf" },
      value: 0,
      right: { __name: "Leaf" },
    }) == 1)
  ) throw new Error("expect failed");
}

function test_empty(): void {
  if (!(maxDepth({ __name: "Leaf" }) == 0)) throw new Error("expect failed");
}

function main(): void {
  test_example_1();
  test_example_2();
  test_single_node();
  test_empty();
}
function _max(v: any): number {
  let list: any[] | null = null;
  if (Array.isArray(v)) list = v;
  else if (v && typeof v === "object") {
    if (Array.isArray((v as any).items)) list = (v as any).items;
    else if (Array.isArray((v as any).Items)) list = (v as any).Items;
  }
  if (!list || list.length === 0) return 0;
  let m = Number(list[0]);
  for (const n of list) {
    const num = Number(n);
    if (num > m) m = num;
  }
  return m;
}

main();
