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

int value_of_root(Tree t) {
  return (() {
  var _t = t;
  if (_t is Node) { return ((v) { return v; })((_t as Node).value); }
  return 0;
})();
}

void main() {
  print(value_of_root(Node(left: Leaf(), value: 5, right: Leaf())));
}
