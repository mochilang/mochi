Map<String, Function> _structParsers = {};

abstract class Tree {}
class Leaf extends Tree {
  Leaf();
}
class Node extends Tree {
  Tree left;
  int value;
  Tree right;
  Node({required this.left, required this.value, required this.right});
}

int depth(Tree t) {
  return (() {
  var _t = t;
  if (_t is Leaf) { return 0; }
  if (_t is Node) { return ((l, r) { return ((depth(l) + depth(r)) + 1); })((_t as Node).left, (_t as Node).right); }
  return null;
})();
}

void main() {
  print(depth(Node(left: Leaf(), value: 0, right: Leaf())));
}
