public class Main {
    static class TreeNode {
        java.math.BigInteger data;
        java.math.BigInteger left;
        java.math.BigInteger right;
        TreeNode(java.math.BigInteger data, java.math.BigInteger left, java.math.BigInteger right) {
            this.data = data;
            this.left = left;
            this.right = right;
        }
        TreeNode() {}
        @Override public String toString() {
            return String.format("{'data': %s, 'left': %s, 'right': %s}", String.valueOf(data), String.valueOf(left), String.valueOf(right));
        }
    }

    static java.math.BigInteger total_moves = java.math.BigInteger.valueOf(0);

    static java.math.BigInteger count_nodes(TreeNode[] nodes, java.math.BigInteger idx) {
        if (idx.compareTo(java.math.BigInteger.valueOf(0)) == 0) {
            return 0;
        }
        TreeNode node_1 = nodes[(int)(((java.math.BigInteger)(idx)).longValue())];
        return count_nodes(((TreeNode[])(nodes)), new java.math.BigInteger(String.valueOf(node_1.left))).add(count_nodes(((TreeNode[])(nodes)), new java.math.BigInteger(String.valueOf(node_1.right)))).add(java.math.BigInteger.valueOf(1));
    }

    static java.math.BigInteger count_coins(TreeNode[] nodes, java.math.BigInteger idx) {
        if (idx.compareTo(java.math.BigInteger.valueOf(0)) == 0) {
            return 0;
        }
        TreeNode node_3 = nodes[(int)(((java.math.BigInteger)(idx)).longValue())];
        return count_coins(((TreeNode[])(nodes)), new java.math.BigInteger(String.valueOf(node_3.left))).add(count_coins(((TreeNode[])(nodes)), new java.math.BigInteger(String.valueOf(node_3.right)))).add(node_3.data);
    }

    static java.math.BigInteger iabs(java.math.BigInteger x) {
        if (x.compareTo(java.math.BigInteger.valueOf(0)) < 0) {
            return (x).negate();
        }
        return x;
    }

    static java.math.BigInteger dfs(TreeNode[] nodes, java.math.BigInteger idx) {
        if (idx.compareTo(java.math.BigInteger.valueOf(0)) == 0) {
            return 0;
        }
        TreeNode node_5 = nodes[(int)(((java.math.BigInteger)(idx)).longValue())];
        java.math.BigInteger left_excess_1 = new java.math.BigInteger(String.valueOf(dfs(((TreeNode[])(nodes)), new java.math.BigInteger(String.valueOf(node_5.left)))));
        java.math.BigInteger right_excess_1 = new java.math.BigInteger(String.valueOf(dfs(((TreeNode[])(nodes)), new java.math.BigInteger(String.valueOf(node_5.right)))));
        java.math.BigInteger abs_left_1 = new java.math.BigInteger(String.valueOf(iabs(new java.math.BigInteger(String.valueOf(left_excess_1)))));
        java.math.BigInteger abs_right_1 = new java.math.BigInteger(String.valueOf(iabs(new java.math.BigInteger(String.valueOf(right_excess_1)))));
        total_moves = new java.math.BigInteger(String.valueOf(total_moves.add(abs_left_1).add(abs_right_1)));
        return node_5.data.add(left_excess_1).add(right_excess_1).subtract(java.math.BigInteger.valueOf(1));
    }

    static java.math.BigInteger distribute_coins(TreeNode[] nodes, java.math.BigInteger root) {
        if (root.compareTo(java.math.BigInteger.valueOf(0)) == 0) {
            return 0;
        }
        if (count_nodes(((TreeNode[])(nodes)), new java.math.BigInteger(String.valueOf(root))).compareTo(count_coins(((TreeNode[])(nodes)), new java.math.BigInteger(String.valueOf(root)))) != 0) {
            throw new RuntimeException(String.valueOf("The nodes number should be same as the number of coins"));
        }
        total_moves = java.math.BigInteger.valueOf(0);
        dfs(((TreeNode[])(nodes)), new java.math.BigInteger(String.valueOf(root)));
        return total_moves;
    }

    static void main() {
        TreeNode[] example1 = ((TreeNode[])(new TreeNode[]{new TreeNode(0, 0, 0), new TreeNode(3, 2, 3), new TreeNode(0, 0, 0), new TreeNode(0, 0, 0)}));
        TreeNode[] example2_1 = ((TreeNode[])(new TreeNode[]{new TreeNode(0, 0, 0), new TreeNode(0, 2, 3), new TreeNode(3, 0, 0), new TreeNode(0, 0, 0)}));
        TreeNode[] example3_1 = ((TreeNode[])(new TreeNode[]{new TreeNode(0, 0, 0), new TreeNode(0, 2, 3), new TreeNode(0, 0, 0), new TreeNode(3, 0, 0)}));
        System.out.println(distribute_coins(((TreeNode[])(example1)), java.math.BigInteger.valueOf(1)));
        System.out.println(distribute_coins(((TreeNode[])(example2_1)), java.math.BigInteger.valueOf(1)));
        System.out.println(distribute_coins(((TreeNode[])(example3_1)), java.math.BigInteger.valueOf(1)));
        System.out.println(distribute_coins(((TreeNode[])(new TreeNode[]{new TreeNode(0, 0, 0)})), java.math.BigInteger.valueOf(0)));
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
