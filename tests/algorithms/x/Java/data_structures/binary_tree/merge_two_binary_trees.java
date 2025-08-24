public class Main {
    interface Tree {}

    static class Leaf implements Tree {
        Leaf() {
        }
        Leaf() {}
        @Override public String toString() {
            return "Leaf{}";
        }
    }

    static class Node implements Tree {
        Tree left;
        java.math.BigInteger value;
        Tree right;
        Node(Tree left, java.math.BigInteger value, Tree right) {
            this.left = left;
            this.value = value;
            this.right = right;
        }
        Node() {}
        @Override public String toString() {
            return String.format("{'left': %s, 'value': %s, 'right': %s}", String.valueOf(left), String.valueOf(value), String.valueOf(right));
        }
    }

    static Node tree1;
    static Node tree2;
    static Tree merged_tree;

    static Tree merge_two_binary_trees(Tree t1, Tree t2) {
        return t1 instanceof Leaf ? t2 : t2 instanceof Leaf ? t1 : new Node(merge_two_binary_trees(l1, l2), ((Number)(v1)).intValue() + ((Number)(v2)).intValue(), merge_two_binary_trees(r1, r2));
    }

    static boolean is_leaf(Tree t) {
        return t instanceof Leaf ? true : false;
    }

    static Tree get_left(Tree t) {
        return ((Tree)(t instanceof Node ? ((Node)(t)).left : new Leaf()));
    }

    static Tree get_right(Tree t) {
        return ((Tree)(t instanceof Node ? ((Node)(t)).right : new Leaf()));
    }

    static java.math.BigInteger get_value(Tree t) {
        return t instanceof Node ? ((Node)(t)).value : 0;
    }

    static void print_preorder(Tree t) {
        if (!(Boolean)is_leaf(t)) {
            java.math.BigInteger v = new java.math.BigInteger(String.valueOf(get_value(t)));
            Tree l = get_left(t);
            Tree r = get_right(t);
            System.out.println(v);
            print_preorder(l);
            print_preorder(r);
        }
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            tree1 = new Node(new Node(new Node(new Leaf(), 4, new Leaf()), 2, new Leaf()), 1, new Node(new Leaf(), 3, new Leaf()));
            tree2 = new Node(new Node(new Leaf(), 4, new Node(new Leaf(), 9, new Leaf())), 2, new Node(new Leaf(), 6, new Node(new Leaf(), 5, new Leaf())));
            System.out.println("Tree1 is:");
            print_preorder(tree1);
            System.out.println("Tree2 is:");
            print_preorder(tree2);
            merged_tree = merge_two_binary_trees(tree1, tree2);
            System.out.println("Merged Tree is:");
            print_preorder(merged_tree);
            long _benchDuration = _now() - _benchStart;
            long _benchMemory = _mem() - _benchMem;
            System.out.println("{\"duration_us\": " + _benchDuration + ", \"memory_bytes\": " + _benchMemory + ", \"name\": \"main\"}");
            return;
        }
    }

    static boolean _nowSeeded = false;
    static int _nowSeed;
    static int _now() {
        if (!_nowSeeded) {
            String s = System.getenv("MOCHI_NOW_SEED");
            if (s != null && !s.isEmpty()) {
                try { _nowSeed = Integer.parseInt(s); _nowSeeded = true; } catch (Exception e) {}
            }
        }
        if (_nowSeeded) {
            _nowSeed = (int)((_nowSeed * 1664525L + 1013904223) % 2147483647);
            return _nowSeed;
        }
        return (int)(System.nanoTime() / 1000);
    }

    static long _mem() {
        Runtime rt = Runtime.getRuntime();
        rt.gc();
        return rt.totalMemory() - rt.freeMemory();
    }
}
