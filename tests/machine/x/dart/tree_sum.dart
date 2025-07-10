abstract class Tree {}
class Leaf extends Tree {}
class Node extends Tree {
  Tree left;
  int value;
  Tree right;
  Node(this.left, this.value, this.right);
}

int sumTree(Tree t) {
  if (t is Leaf) return 0;
  if (t is Node) {
    return sumTree(t.left) + t.value + sumTree(t.right);
  }
  return 0;
}

void main() {
  var t = Node(Leaf(), 1, Node(Leaf(), 2, Leaf()));
  print(sumTree(t));
}
