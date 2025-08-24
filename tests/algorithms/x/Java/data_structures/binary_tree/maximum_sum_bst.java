public class Main {
    static class Node {
        java.math.BigInteger val;
        java.math.BigInteger left;
        java.math.BigInteger right;
        Node(java.math.BigInteger val, java.math.BigInteger left, java.math.BigInteger right) {
            this.val = val;
            this.left = left;
            this.right = right;
        }
        Node() {}
        @Override public String toString() {
            return String.format("{'val': %s, 'left': %s, 'right': %s}", String.valueOf(val), String.valueOf(left), String.valueOf(right));
        }
    }

    static class Info {
        boolean is_bst;
        java.math.BigInteger min_val;
        java.math.BigInteger max_val;
        java.math.BigInteger total;
        java.math.BigInteger best;
        Info(boolean is_bst, java.math.BigInteger min_val, java.math.BigInteger max_val, java.math.BigInteger total, java.math.BigInteger best) {
            this.is_bst = is_bst;
            this.min_val = min_val;
            this.max_val = max_val;
            this.total = total;
            this.best = best;
        }
        Info() {}
        @Override public String toString() {
            return String.format("{'is_bst': %s, 'min_val': %s, 'max_val': %s, 'total': %s, 'best': %s}", String.valueOf(is_bst), String.valueOf(min_val), String.valueOf(max_val), String.valueOf(total), String.valueOf(best));
        }
    }


    static java.math.BigInteger min_int(java.math.BigInteger a, java.math.BigInteger b) {
        if (a.compareTo(b) < 0) {
            return a;
        }
        return b;
    }

    static java.math.BigInteger max_int(java.math.BigInteger a, java.math.BigInteger b) {
        if (a.compareTo(b) > 0) {
            return a;
        }
        return b;
    }

    static Info solver(Node[] nodes, java.math.BigInteger idx) {
        if (idx.compareTo((java.math.BigInteger.valueOf(1)).negate()) == 0) {
            return new Info(true, 2147483647, (java.math.BigInteger.valueOf(2147483648)).negate(), 0, 0);
        }
        Node node_1 = nodes[(int)(((java.math.BigInteger)(idx)).longValue())];
        Info left_info_1 = solver(((Node[])(nodes)), new java.math.BigInteger(String.valueOf(node_1.left)));
        Info right_info_1 = solver(((Node[])(nodes)), new java.math.BigInteger(String.valueOf(node_1.right)));
        java.math.BigInteger current_best_1 = new java.math.BigInteger(String.valueOf(max_int(new java.math.BigInteger(String.valueOf(left_info_1.best)), new java.math.BigInteger(String.valueOf(right_info_1.best)))));
        if (left_info_1.is_bst && right_info_1.is_bst && left_info_1.max_val.compareTo(node_1.val) < 0 && node_1.val.compareTo(right_info_1.min_val) < 0) {
            java.math.BigInteger sum_val_1 = new java.math.BigInteger(String.valueOf(left_info_1.total.add(right_info_1.total).add(node_1.val)));
            current_best_1 = new java.math.BigInteger(String.valueOf(max_int(new java.math.BigInteger(String.valueOf(current_best_1)), new java.math.BigInteger(String.valueOf(sum_val_1)))));
            return new Info(true, min_int(new java.math.BigInteger(String.valueOf(left_info_1.min_val)), new java.math.BigInteger(String.valueOf(node_1.val))), max_int(new java.math.BigInteger(String.valueOf(right_info_1.max_val)), new java.math.BigInteger(String.valueOf(node_1.val))), sum_val_1, current_best_1);
        }
        return new Info(false, 0, 0, 0, current_best_1);
    }

    static java.math.BigInteger max_sum_bst(Node[] nodes, java.math.BigInteger root) {
        Info info = solver(((Node[])(nodes)), new java.math.BigInteger(String.valueOf(root)));
        return info.best;
    }

    static void main() {
        Node[] t1_nodes = ((Node[])(new Node[]{new Node(4, 1, (java.math.BigInteger.valueOf(1)).negate()), new Node(3, 2, 3), new Node(1, (java.math.BigInteger.valueOf(1)).negate(), (java.math.BigInteger.valueOf(1)).negate()), new Node(2, (java.math.BigInteger.valueOf(1)).negate(), (java.math.BigInteger.valueOf(1)).negate())}));
        System.out.println(max_sum_bst(((Node[])(t1_nodes)), java.math.BigInteger.valueOf(0)));
        Node[] t2_nodes_1 = ((Node[])(new Node[]{new Node((java.math.BigInteger.valueOf(4)).negate(), 1, 2), new Node((java.math.BigInteger.valueOf(2)).negate(), (java.math.BigInteger.valueOf(1)).negate(), (java.math.BigInteger.valueOf(1)).negate()), new Node((java.math.BigInteger.valueOf(5)).negate(), (java.math.BigInteger.valueOf(1)).negate(), (java.math.BigInteger.valueOf(1)).negate())}));
        System.out.println(max_sum_bst(((Node[])(t2_nodes_1)), java.math.BigInteger.valueOf(0)));
        Node[] t3_nodes_1 = ((Node[])(new Node[]{new Node(1, 1, 2), new Node(4, 3, 4), new Node(3, 5, 6), new Node(2, (java.math.BigInteger.valueOf(1)).negate(), (java.math.BigInteger.valueOf(1)).negate()), new Node(4, (java.math.BigInteger.valueOf(1)).negate(), (java.math.BigInteger.valueOf(1)).negate()), new Node(2, (java.math.BigInteger.valueOf(1)).negate(), (java.math.BigInteger.valueOf(1)).negate()), new Node(5, 7, 8), new Node(4, (java.math.BigInteger.valueOf(1)).negate(), (java.math.BigInteger.valueOf(1)).negate()), new Node(6, (java.math.BigInteger.valueOf(1)).negate(), (java.math.BigInteger.valueOf(1)).negate())}));
        System.out.println(max_sum_bst(((Node[])(t3_nodes_1)), java.math.BigInteger.valueOf(0)));
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
