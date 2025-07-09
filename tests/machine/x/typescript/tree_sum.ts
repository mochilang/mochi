type Tree = { kind: "leaf" } | { kind: "node"; left: any; value: any; right: any };
function sum_tree(t) {
  return (() => {
  const _tmp1 = t;
  let _res;
  if (_tmp1.kind === "leaf") {
    _res = 0;
  }
  else if (_tmp1.kind === "node") {
    const left = _tmp1.left;
    const value = _tmp1.value;
    const right = _tmp1.right;
    _res = ((sum_tree(left) + value) + sum_tree(right));
  }
  else { _res = undefined }
  return _res;
})()
;
}
let t = {kind: "node", left: {kind: "leaf"}, value: 1, right: {kind: "node", left: {kind: "leaf"}, value: 2, right: {kind: "leaf"}}};
console.log(sum_tree(t));
