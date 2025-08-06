public class Main {
    static class Node {
        int val;
        int left;
        int right;
        Node(int val, int left, int right) {
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
        int min_val;
        int max_val;
        int total;
        int best;
        Info(boolean is_bst, int min_val, int max_val, int total, int best) {
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


    static int min_int(int a, int b) {
        if (a < b) {
            return a;
        }
        return b;
    }

    static int max_int(int a, int b) {
        if (a > b) {
            return a;
        }
        return b;
    }

    static Info solver(Node[] nodes, int idx) {
        if (idx == 0 - 1) {
            return new Info(true, 2147483647, -(int)2147483648L, 0, 0);
        }
        Node node = nodes[idx];
        Info left_info = solver(((Node[])(nodes)), node.left);
        Info right_info = solver(((Node[])(nodes)), node.right);
        int current_best = max_int(left_info.best, right_info.best);
        if (left_info.is_bst && right_info.is_bst && left_info.max_val < node.val && node.val < right_info.min_val) {
            int sum_val = left_info.total + right_info.total + node.val;
            current_best = max_int(current_best, sum_val);
            return new Info(true, min_int(left_info.min_val, node.val), max_int(right_info.max_val, node.val), sum_val, current_best);
        }
        return new Info(false, 0, 0, 0, current_best);
    }

    static int max_sum_bst(Node[] nodes, int root) {
        Info info = solver(((Node[])(nodes)), root);
        return info.best;
    }

    static void main() {
        Node[] t1_nodes = ((Node[])(new Node[]{new Node(4, 1, 0 - 1), new Node(3, 2, 3), new Node(1, 0 - 1, 0 - 1), new Node(2, 0 - 1, 0 - 1)}));
        System.out.println(max_sum_bst(((Node[])(t1_nodes)), 0));
        Node[] t2_nodes = ((Node[])(new Node[]{new Node(-4, 1, 2), new Node(-2, 0 - 1, 0 - 1), new Node(-5, 0 - 1, 0 - 1)}));
        System.out.println(max_sum_bst(((Node[])(t2_nodes)), 0));
        Node[] t3_nodes = ((Node[])(new Node[]{new Node(1, 1, 2), new Node(4, 3, 4), new Node(3, 5, 6), new Node(2, 0 - 1, 0 - 1), new Node(4, 0 - 1, 0 - 1), new Node(2, 0 - 1, 0 - 1), new Node(5, 7, 8), new Node(4, 0 - 1, 0 - 1), new Node(6, 0 - 1, 0 - 1)}));
        System.out.println(max_sum_bst(((Node[])(t3_nodes)), 0));
    }
    public static void main(String[] args) {
        main();
    }
}
