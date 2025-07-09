type Tree = { kind: 'Leaf' } | { kind: 'Node'; left: any; value: any; right: any };
function sum_tree(t) {
  return (() => {
  const _tmp1 = t;
  let _res;
  if (_tmp1.kind === 'Leaf') {
    _res = 0;
  }
  else if (_tmp1.kind === 'Node') {
    const left = _tmp1.left;
    const value = _tmp1.value;
    const right = _tmp1.right;
    _res = ((sum_tree(left) + value) + sum_tree(right));
  }
  return _res;
})()
;
}
let t = {kind: 'Node', left: { kind: 'Leaf' }, value: 1, right: {kind: 'Node', left: { kind: 'Leaf' }, value: 2, right: { kind: 'Leaf' }}};
console.log(sum_tree(t));
