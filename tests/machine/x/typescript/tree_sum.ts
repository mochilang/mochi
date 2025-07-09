type Tree = { kind: "leaf" } | { kind: "node"; left: any; value: any; right: any };
function sum_tree(t) {
  return (() => {
  const _tmp1 = t;
  let _res;
  switch (_tmp1) {
    case Leaf:
      _res = 0;
      break;
    case Node(left, value, right):
      _res = ((sum_tree(left) + value) + sum_tree(right));
      break;
    default:
      _res = undefined;
      break;
  }
  return _res;
})()
;
}
let t = {left: Leaf, value: 1, right: {left: Leaf, value: 2, right: Leaf}};
console.log(sum_tree(t));
