abstract class Tree {}
class Leaf extends Tree {
  Leaf();
}
class Node extends Tree {
  Tree left;
  int value;
  Tree right;
  Node(this.left, this.value, this.right);
}

int sum_tree(Tree t) {
  return (() {
  var _t = t;
  if (_t is Leaf) {
    return 0;
  } else if (_t is Node) {
    var left = (_t as Node).left;
    var value = (_t as Node).value;
    var right = (_t as Node).right;
    return (sum_tree(left) + (value as num) as num) + sum_tree(right);
  }  return null;
})();
}

var t = Node(Leaf(), 1, Node(Leaf(), 2, Leaf()));

void main() {
  print(sum_tree(t));
}
