public class Main {
    static class Node {
        int minn;
        int maxx;
        int[] map_left;
        int left;
        int right;
        Node(int minn, int maxx, int[] map_left, int left, int right) {
            this.minn = minn;
            this.maxx = maxx;
            this.map_left = map_left;
            this.left = left;
            this.right = right;
        }
        Node() {}
        @Override public String toString() {
            return String.format("{'minn': %s, 'maxx': %s, 'map_left': %s, 'left': %s, 'right': %s}", String.valueOf(minn), String.valueOf(maxx), String.valueOf(map_left), String.valueOf(left), String.valueOf(right));
        }
    }

    static Node[] nodes = new Node[0];
    static int[] test_array;
    static int root;

    static int[] make_list(int length, int value) {
        int[] lst = ((int[])(new int[]{}));
        int i = 0;
        while (i < length) {
            lst = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(lst), java.util.stream.IntStream.of(value)).toArray()));
            i = i + 1;
        }
        return lst;
    }

    static int min_list(int[] arr) {
        int m = arr[0];
        int i_1 = 1;
        while (i_1 < arr.length) {
            if (arr[i_1] < m) {
                m = arr[i_1];
            }
            i_1 = i_1 + 1;
        }
        return m;
    }

    static int max_list(int[] arr) {
        int m_1 = arr[0];
        int i_2 = 1;
        while (i_2 < arr.length) {
            if (arr[i_2] > m_1) {
                m_1 = arr[i_2];
            }
            i_2 = i_2 + 1;
        }
        return m_1;
    }

    static int build_tree(int[] arr) {
        Node n = new Node(min_list(((int[])(arr))), max_list(((int[])(arr))), make_list(arr.length, 0), -1, -1);
        if (n.minn == n.maxx) {
            nodes = ((Node[])(java.util.stream.Stream.concat(java.util.Arrays.stream(nodes), java.util.stream.Stream.of(n)).toArray(Node[]::new)));
            return nodes.length - 1;
        }
        Object pivot = Math.floorDiv((n.minn + n.maxx), 2);
        int[] left_arr = ((int[])(new int[]{}));
        int[] right_arr = ((int[])(new int[]{}));
        int i_3 = 0;
        while (i_3 < arr.length) {
            int num = arr[i_3];
            if (num <= ((Number)(pivot)).intValue()) {
                left_arr = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(left_arr), java.util.stream.IntStream.of(num)).toArray()));
            } else {
                right_arr = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(right_arr), java.util.stream.IntStream.of(num)).toArray()));
            }
            int[] ml = ((int[])(n.map_left));
ml[i_3] = left_arr.length;
n.map_left = ml;
            i_3 = i_3 + 1;
        }
        if (left_arr.length > 0) {
n.left = build_tree(((int[])(left_arr)));
        }
        if (right_arr.length > 0) {
n.right = build_tree(((int[])(right_arr)));
        }
        nodes = ((Node[])(java.util.stream.Stream.concat(java.util.Arrays.stream(nodes), java.util.stream.Stream.of(n)).toArray(Node[]::new)));
        return nodes.length - 1;
    }

    static int rank_till_index(int node_idx, int num, int index) {
        if (index < 0 || node_idx < 0) {
            return 0;
        }
        Node node = nodes[node_idx];
        if (node.minn == node.maxx) {
            if (node.minn == num) {
                return index + 1;
            } else {
                return 0;
            }
        }
        Object pivot_1 = Math.floorDiv((node.minn + node.maxx), 2);
        if (num <= ((Number)(pivot_1)).intValue()) {
            return rank_till_index(node.left, num, node.map_left[index] - 1);
        } else {
            return rank_till_index(node.right, num, index - node.map_left[index]);
        }
    }

    static int rank(int node_idx, int num, int start, int end) {
        if (start > end) {
            return 0;
        }
        int rank_till_end = rank_till_index(node_idx, num, end);
        int rank_before_start = rank_till_index(node_idx, num, start - 1);
        return rank_till_end - rank_before_start;
    }

    static int quantile(int node_idx, int index, int start, int end) {
        if (index > (end - start) || start > end || node_idx < 0) {
            return -1;
        }
        Node node_1 = nodes[node_idx];
        if (node_1.minn == node_1.maxx) {
            return node_1.minn;
        }
        int left_start = start == 0 ? 0 : node_1.map_left[start - 1];
        int num_left = node_1.map_left[end] - left_start;
        if (num_left > index) {
            return quantile(node_1.left, index, left_start, node_1.map_left[end] - 1);
        } else {
            return quantile(node_1.right, index - num_left, start - left_start, end - node_1.map_left[end]);
        }
    }

    static int range_counting(int node_idx, int start, int end, int start_num, int end_num) {
        if (start > end || node_idx < 0 || start_num > end_num) {
            return 0;
        }
        Node node_2 = nodes[node_idx];
        if (node_2.minn > end_num || node_2.maxx < start_num) {
            return 0;
        }
        if (start_num <= node_2.minn && node_2.maxx <= end_num) {
            return end - start + 1;
        }
        int left = range_counting(node_2.left, start == 0 ? 0 : node_2.map_left[start - 1], node_2.map_left[end] - 1, start_num, end_num);
        int right = range_counting(node_2.right, start - (start == 0 ? 0 : node_2.map_left[start - 1]), end - node_2.map_left[end], start_num, end_num);
        return left + right;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            nodes = ((Node[])(new Node[]{}));
            test_array = ((int[])(new int[]{2, 1, 4, 5, 6, 0, 8, 9, 1, 2, 0, 6, 4, 2, 0, 6, 5, 3, 2, 7}));
            root = build_tree(((int[])(test_array)));
            System.out.println("rank_till_index 6 at 6 -> " + _p(rank_till_index(root, 6, 6)));
            System.out.println("rank 6 in [3,13] -> " + _p(rank(root, 6, 3, 13)));
            System.out.println("quantile index 2 in [2,5] -> " + _p(quantile(root, 2, 2, 5)));
            System.out.println("range_counting [3,7] in [1,10] -> " + _p(range_counting(root, 1, 10, 3, 7)));
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
}
