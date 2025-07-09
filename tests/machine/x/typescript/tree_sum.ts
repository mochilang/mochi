interface Leaf {
  kind: "Leaf";
}

interface Node {
  kind: "Node";
  left: any;
  value: any;
  right: any;
}

type Tree = Leaf | Node;
function sum_tree(t) {
  return (() => {
  const _tmp1 = t;
  let _res;
  switch (_tmp1.kind) {
    case "Leaf":
      _res = 0;
      break;
    case "Node":
      const left = _tmp1.left;
      const value = _tmp1.value;
      const right = _tmp1.right;
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
let t = {kind: "Node", left: { kind: "Leaf" }, value: 1, right: {kind: "Node", left: { kind: "Leaf" }, value: 2, right: { kind: "Leaf" }}};
console.log(sum_tree(t));
