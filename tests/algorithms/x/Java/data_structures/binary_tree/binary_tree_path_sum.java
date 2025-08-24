public class Main {
    interface Tree {}

    static class Empty implements Tree {
        Empty() {
        }
        Empty() {}
        @Override public String toString() {
            return "Empty{}";
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


    static java.math.BigInteger dfs(Tree node, java.math.BigInteger target, java.math.BigInteger current) {
        return node instanceof Empty ? 0 : (current.add(new java.math.BigInteger(String.valueOf(v))).compareTo(target) == 0 ? 1 : 0).add(dfs(((Node)(node)).left, new java.math.BigInteger(String.valueOf(target)), new java.math.BigInteger(String.valueOf(current.add(new java.math.BigInteger(String.valueOf(((Node)(node)).value))))))).add(dfs(((Node)(node)).right, new java.math.BigInteger(String.valueOf(target)), new java.math.BigInteger(String.valueOf(current.add(new java.math.BigInteger(String.valueOf(((Node)(node)).value)))))));
    }

    static java.math.BigInteger path_sum(Tree node, java.math.BigInteger target) {
        return node instanceof Empty ? 0 : dfs(node, new java.math.BigInteger(String.valueOf(target)), java.math.BigInteger.valueOf(0)).add(path_sum(((Node)(node)).left, new java.math.BigInteger(String.valueOf(target)))).add(path_sum(((Node)(node)).right, new java.math.BigInteger(String.valueOf(target))));
    }

    static Tree sample_tree_one() {
        return ((Tree)(new Node(new Node(new Node(new Node(Empty, 3, Empty), 3, new Node(Empty, (java.math.BigInteger.valueOf(2)).negate(), Empty)), 5, new Node(Empty, 2, new Node(Empty, 1, Empty))), 10, new Node(Empty, (java.math.BigInteger.valueOf(3)).negate(), new Node(Empty, 11, Empty)))));
    }

    static Tree sample_tree_two() {
        return ((Tree)(new Node(new Node(new Node(new Node(Empty, 3, Empty), 3, new Node(Empty, (java.math.BigInteger.valueOf(2)).negate(), Empty)), 5, new Node(Empty, 2, new Node(Empty, 1, Empty))), 10, new Node(Empty, (java.math.BigInteger.valueOf(3)).negate(), new Node(Empty, 10, Empty)))));
    }

    static void main() {
        Tree tree1 = sample_tree_one();
        System.out.println(path_sum(tree1, java.math.BigInteger.valueOf(8)));
        System.out.println(path_sum(tree1, java.math.BigInteger.valueOf(7)));
        Tree tree2_1 = sample_tree_two();
        System.out.println(path_sum(tree2_1, java.math.BigInteger.valueOf(8)));
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            main();
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
