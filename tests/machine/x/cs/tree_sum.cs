using System;

public interface Tree { void isTree(); }
public struct Leaf : Tree {
    public void isTree() {}
}
public struct Node : Tree {
    public Tree left;
    public int value;
    public Tree right;
    public void isTree() {}
}

class Program {
    static int sum_tree(Tree t) {
        return new Func<dynamic>(() => {
        var _t = t;
        if (_t is Leaf) return 0;
        if (_t is Node _tmp0) {
            var left = _tmp0.left;
            var value = _tmp0.value;
            var right = _tmp0.right;
            return ((sum_tree(left) + value) + sum_tree(right));
        }
        return null;
    })();
    }
    
    static void Main() {
        Node t = new Node { left = Leaf, value = 1, right = new Node { left = Leaf, value = 2, right = Leaf } };
        Console.WriteLine(sum_tree(t));
    }
}
