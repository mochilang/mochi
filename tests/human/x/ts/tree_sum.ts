type Tree = { kind: 'leaf' } | { kind: 'node'; left: Tree; value: number; right: Tree };

function sumTree(t: Tree): number {
  if (t.kind === 'leaf') {
    return 0;
  }
  return sumTree(t.left) + t.value + sumTree(t.right);
}

const t: Tree = {
  kind: 'node',
  left: { kind: 'leaf' },
  value: 1,
  right: {
    kind: 'node',
    left: { kind: 'leaf' },
    value: 2,
    right: { kind: 'leaf' }
  }
};

console.log(sumTree(t));
