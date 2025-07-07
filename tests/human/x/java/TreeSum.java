class Tree {
    static class Leaf extends Tree {}
    static class Node extends Tree {
        Tree left;
        int value;
        Tree right;
        Node(Tree left, int value, Tree right) {
            this.left = left;
            this.value = value;
            this.right = right;
        }
    }
}

public class TreeSum {
    static int sumTree(Tree t) {
        if (t instanceof Tree.Leaf) {
            return 0;
        }
        Tree.Node n = (Tree.Node) t;
        return sumTree(n.left) + n.value + sumTree(n.right);
    }

    public static void main(String[] args) {
        Tree t = new Tree.Node(new Tree.Leaf(), 1,
                new Tree.Node(new Tree.Leaf(), 2, new Tree.Leaf()));
        System.out.println(sumTree(t));
    }
}
