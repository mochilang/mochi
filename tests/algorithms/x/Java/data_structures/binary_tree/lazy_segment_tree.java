public class Main {
    static int NEG_INF;
    static int[] A;
    static int n;
    static int[] segment_tree = new int[0];
    static int[] lazy = new int[0];
    static boolean[] flag = new boolean[0];

    static int[] init_int_array(int n) {
        int[] arr = ((int[])(new int[]{}));
        int i = 0;
        while (i < 4 * n + 5) {
            arr = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(arr), java.util.stream.IntStream.of(0)).toArray()));
            i = i + 1;
        }
        return arr;
    }

    static boolean[] init_bool_array(int n) {
        boolean[] arr_1 = ((boolean[])(new boolean[]{}));
        int i_1 = 0;
        while (i_1 < 4 * n + 5) {
            arr_1 = ((boolean[])(appendBool(arr_1, false)));
            i_1 = i_1 + 1;
        }
        return arr_1;
    }

    static int left(int idx) {
        return idx * 2;
    }

    static int right(int idx) {
        return idx * 2 + 1;
    }

    static void build(int[] segment_tree, int idx, int l, int r, int[] a) {
        if (l == r) {
segment_tree[idx] = a[l - 1];
        } else {
            int mid = (l + r) / 2;
            build(((int[])(segment_tree)), left(idx), l, mid, ((int[])(a)));
            build(((int[])(segment_tree)), right(idx), mid + 1, r, ((int[])(a)));
            int lv = segment_tree[left(idx)];
            int rv = segment_tree[right(idx)];
            if (lv > rv) {
segment_tree[idx] = lv;
            } else {
segment_tree[idx] = rv;
            }
        }
    }

    static void update(int[] segment_tree, int[] lazy, boolean[] flag, int idx, int l, int r, int a, int b, int val) {
        if (((Boolean)(flag[idx]))) {
segment_tree[idx] = lazy[idx];
flag[idx] = false;
            if (l != r) {
lazy[left(idx)] = lazy[idx];
lazy[right(idx)] = lazy[idx];
flag[left(idx)] = true;
flag[right(idx)] = true;
            }
        }
        if (r < a || l > b) {
            return;
        }
        if (l >= a && r <= b) {
segment_tree[idx] = val;
            if (l != r) {
lazy[left(idx)] = val;
lazy[right(idx)] = val;
flag[left(idx)] = true;
flag[right(idx)] = true;
            }
            return;
        }
        int mid_1 = (l + r) / 2;
        update(((int[])(segment_tree)), ((int[])(lazy)), ((boolean[])(flag)), left(idx), l, mid_1, a, b, val);
        update(((int[])(segment_tree)), ((int[])(lazy)), ((boolean[])(flag)), right(idx), mid_1 + 1, r, a, b, val);
        int lv_1 = segment_tree[left(idx)];
        int rv_1 = segment_tree[right(idx)];
        if (lv_1 > rv_1) {
segment_tree[idx] = lv_1;
        } else {
segment_tree[idx] = rv_1;
        }
    }

    static int query(int[] segment_tree, int[] lazy, boolean[] flag, int idx, int l, int r, int a, int b) {
        if (((Boolean)(flag[idx]))) {
segment_tree[idx] = lazy[idx];
flag[idx] = false;
            if (l != r) {
lazy[left(idx)] = lazy[idx];
lazy[right(idx)] = lazy[idx];
flag[left(idx)] = true;
flag[right(idx)] = true;
            }
        }
        if (r < a || l > b) {
            return NEG_INF;
        }
        if (l >= a && r <= b) {
            return segment_tree[idx];
        }
        int mid_2 = (l + r) / 2;
        int q1 = query(((int[])(segment_tree)), ((int[])(lazy)), ((boolean[])(flag)), left(idx), l, mid_2, a, b);
        int q2 = query(((int[])(segment_tree)), ((int[])(lazy)), ((boolean[])(flag)), right(idx), mid_2 + 1, r, a, b);
        if (q1 > q2) {
            return q1;
        } else {
            return q2;
        }
    }

    static String segtree_to_string(int[] segment_tree, int[] lazy, boolean[] flag, int n) {
        String res = "[";
        int i_2 = 1;
        while (i_2 <= n) {
            int v = query(((int[])(segment_tree)), ((int[])(lazy)), ((boolean[])(flag)), 1, 1, n, i_2, i_2);
            res = res + _p(v);
            if (i_2 < n) {
                res = res + ", ";
            }
            i_2 = i_2 + 1;
        }
        res = res + "]";
        return res;
    }
    public static void main(String[] args) {
        NEG_INF = -1000000000;
        A = ((int[])(new int[]{1, 2, -4, 7, 3, -5, 6, 11, -20, 9, 14, 15, 5, 2, -8}));
        n = 15;
        segment_tree = ((int[])(init_int_array(n)));
        lazy = ((int[])(init_int_array(n)));
        flag = ((boolean[])(init_bool_array(n)));
        build(((int[])(segment_tree)), 1, 1, n, ((int[])(A)));
        System.out.println(query(((int[])(segment_tree)), ((int[])(lazy)), ((boolean[])(flag)), 1, 1, n, 4, 6));
        System.out.println(query(((int[])(segment_tree)), ((int[])(lazy)), ((boolean[])(flag)), 1, 1, n, 7, 11));
        System.out.println(query(((int[])(segment_tree)), ((int[])(lazy)), ((boolean[])(flag)), 1, 1, n, 7, 12));
        update(((int[])(segment_tree)), ((int[])(lazy)), ((boolean[])(flag)), 1, 1, n, 1, 3, 111);
        System.out.println(query(((int[])(segment_tree)), ((int[])(lazy)), ((boolean[])(flag)), 1, 1, n, 1, 15));
        update(((int[])(segment_tree)), ((int[])(lazy)), ((boolean[])(flag)), 1, 1, n, 7, 8, 235);
        System.out.println(segtree_to_string(((int[])(segment_tree)), ((int[])(lazy)), ((boolean[])(flag)), n));
    }

    static boolean[] appendBool(boolean[] arr, boolean v) {
        boolean[] out = java.util.Arrays.copyOf(arr, arr.length + 1);
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
}
