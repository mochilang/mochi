public class Main {
    static class Node {
        int key;
        int freq;
        Node(int key, int freq) {
            this.key = key;
            this.freq = freq;
        }
        Node() {}
        @Override public String toString() {
            return String.format("{'key': %s, 'freq': %s}", String.valueOf(key), String.valueOf(freq));
        }
    }


    static Node[] sort_nodes(Node[] nodes) {
        Node[] arr = ((Node[])(nodes));
        int i = 1;
        while (i < arr.length) {
            Node key_node = arr[i];
            int j = i - 1;
            while (j >= 0) {
                Node temp = arr[j];
                if (temp.key > key_node.key) {
arr[j + 1] = temp;
                    j = j - 1;
                } else {
                    break;
                }
            }
arr[j + 1] = key_node;
            i = i + 1;
        }
        return arr;
    }

    static void print_node(Node n) {
        System.out.println("Node(key=" + _p(n.key) + ", freq=" + _p(n.freq) + ")");
    }

    static void print_binary_search_tree(int[][] root, int[] keys, int i, int j, int parent, boolean is_left) {
        if (i > j || i < 0 || j > root.length - 1) {
            return;
        }
        int node = root[i][j];
        if (parent == (-1)) {
            System.out.println(_p(_geti(keys, node)) + " is the root of the binary search tree.");
        } else         if (((Boolean)(is_left))) {
            System.out.println(_p(_geti(keys, node)) + " is the left child of key " + _p(parent) + ".");
        } else {
            System.out.println(_p(_geti(keys, node)) + " is the right child of key " + _p(parent) + ".");
        }
        print_binary_search_tree(((int[][])(root)), ((int[])(keys)), i, node - 1, keys[node], true);
        print_binary_search_tree(((int[][])(root)), ((int[])(keys)), node + 1, j, keys[node], false);
    }

    static void find_optimal_binary_search_tree(Node[] original_nodes) {
        Node[] nodes = ((Node[])(sort_nodes(((Node[])(original_nodes)))));
        int n = nodes.length;
        int[] keys = ((int[])(new int[]{}));
        int[] freqs = ((int[])(new int[]{}));
        int i_1 = 0;
        while (i_1 < n) {
            Node node_1 = nodes[i_1];
            keys = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(keys), java.util.stream.IntStream.of(node_1.key)).toArray()));
            freqs = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(freqs), java.util.stream.IntStream.of(node_1.freq)).toArray()));
            i_1 = i_1 + 1;
        }
        int[][] dp = ((int[][])(new int[][]{}));
        int[][] total = ((int[][])(new int[][]{}));
        int[][] root = ((int[][])(new int[][]{}));
        i_1 = 0;
        while (i_1 < n) {
            int[] dp_row = ((int[])(new int[]{}));
            int[] total_row = ((int[])(new int[]{}));
            int[] root_row = ((int[])(new int[]{}));
            int j_1 = 0;
            while (j_1 < n) {
                if (i_1 == j_1) {
                    dp_row = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(dp_row), java.util.stream.IntStream.of(freqs[i_1])).toArray()));
                    total_row = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(total_row), java.util.stream.IntStream.of(freqs[i_1])).toArray()));
                    root_row = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(root_row), java.util.stream.IntStream.of(i_1)).toArray()));
                } else {
                    dp_row = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(dp_row), java.util.stream.IntStream.of(0)).toArray()));
                    total_row = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(total_row), java.util.stream.IntStream.of(0)).toArray()));
                    root_row = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(root_row), java.util.stream.IntStream.of(0)).toArray()));
                }
                j_1 = j_1 + 1;
            }
            dp = ((int[][])(appendObj(dp, dp_row)));
            total = ((int[][])(appendObj(total, total_row)));
            root = ((int[][])(appendObj(root, root_row)));
            i_1 = i_1 + 1;
        }
        int interval_length = 2;
        int INF = 2147483647;
        while (interval_length <= n) {
            i_1 = 0;
            while (i_1 < n - interval_length + 1) {
                int j_2 = i_1 + interval_length - 1;
dp[i_1][j_2] = INF;
total[i_1][j_2] = total[i_1][j_2 - 1] + freqs[j_2];
                int r = root[i_1][j_2 - 1];
                while (r <= root[i_1 + 1][j_2]) {
                    int left = r != i_1 ? dp[i_1][r - 1] : 0;
                    int right = r != j_2 ? dp[r + 1][j_2] : 0;
                    int cost = left + total[i_1][j_2] + right;
                    if (dp[i_1][j_2] > cost) {
dp[i_1][j_2] = cost;
root[i_1][j_2] = r;
                    }
                    r = r + 1;
                }
                i_1 = i_1 + 1;
            }
            interval_length = interval_length + 1;
        }
        System.out.println("Binary search tree nodes:");
        i_1 = 0;
        while (i_1 < n) {
            print_node(nodes[i_1]);
            i_1 = i_1 + 1;
        }
        System.out.println("\nThe cost of optimal BST for given tree nodes is " + _p(_geti(dp[0], n - 1)) + ".");
        print_binary_search_tree(((int[][])(root)), ((int[])(keys)), 0, n - 1, (-1), false);
    }

    static void main() {
        Node[] nodes_1 = ((Node[])(new Node[]{new Node(12, 8), new Node(10, 34), new Node(20, 50), new Node(42, 3), new Node(25, 40), new Node(37, 30)}));
        find_optimal_binary_search_tree(((Node[])(nodes_1)));
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            main();
            long _benchDuration = _now() - _benchStart;
            long _benchMemory = _mem() - _benchMem;
            System.out.println("{");
            System.out.println("  \"duration_us\": " + _benchDuration + ",");
            System.out.println("  \"memory_bytes\": " + _benchMemory + ",");
            System.out.println("  \"name\": \"main\"");
            System.out.println("}");
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

    static <T> T[] appendObj(T[] arr, T v) {
        T[] out = java.util.Arrays.copyOf(arr, arr.length + 1);
        out[arr.length] = v;
        return out;
    }

    static String _p(Object v) {
        if (v == null) return "<nil>";
        if (v.getClass().isArray()) {
            if (v instanceof int[]) return java.util.Arrays.toString((int[]) v);
            if (v instanceof long[]) return java.util.Arrays.toString((long[]) v);
            if (v instanceof double[]) return java.util.Arrays.toString((double[]) v);
            if (v instanceof boolean[]) return java.util.Arrays.toString((boolean[]) v);
            if (v instanceof byte[]) return java.util.Arrays.toString((byte[]) v);
            if (v instanceof char[]) return java.util.Arrays.toString((char[]) v);
            if (v instanceof short[]) return java.util.Arrays.toString((short[]) v);
            if (v instanceof float[]) return java.util.Arrays.toString((float[]) v);
            return java.util.Arrays.deepToString((Object[]) v);
        }
        return String.valueOf(v);
    }

    static Integer _geti(int[] a, int i) {
        return (i >= 0 && i < a.length) ? a[i] : null;
    }
}
