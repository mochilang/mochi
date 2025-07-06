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

Tree fromList(List<int> nums) {
  Tree helper(int lo, int hi) {
    if ((lo >= hi)) {
      return Leaf();
    }
    int mid = (((lo + hi)) ~/ 2);
    return Node(left: helper(lo, mid), value: nums[mid], right: helper((mid + 1), hi));
  }
  return helper(0, nums.length);
}

List<int> inorder(Tree t) {
  return (() {
  var _t = t;
  if (_t is Leaf) { return []; }
  if (_t is Node) { return ((l, v, r) { return ((inorder(l) + [v]) + inorder(r)); })((_t as Node).left, (_t as Node).value, (_t as Node).right); }
  return null;
})();
}

void main() {
  print(inorder(fromList([-10, -3, 0, 5, 9])));
}
