type Tree = { kind: "leaf" } | { kind: "node"; left: any; value: any; right: any };
function sum_tree(t) {
  return (() => {
  const _tmp63 = t;
  let _res;
  if (_tmp63.kind === "leaf") {
    _res = 0;
  }
  else if (_tmp63.kind === "node") {
    const left = _tmp63.left;
    const value = _tmp63.value;
    const right = _tmp63.right;
    _res = ((sum_tree(left) + value) + sum_tree(right));
  }
  return _res;
})()
;
}
const t = {kind: "node", left: {kind: "leaf"}, value: 1, right: {kind: "node", left: {kind: "leaf"}, value: 2, right: {kind: "leaf"}}};
console.log(sum_tree(t));
