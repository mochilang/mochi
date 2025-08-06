public class Main {
    static class DS {
        int[] parent;
        int[] rank;
        DS(int[] parent, int[] rank) {
            this.parent = parent;
            this.rank = rank;
        }
        DS() {}
        @Override public String toString() {
            return String.format("{'parent': %s, 'rank': %s}", String.valueOf(parent), String.valueOf(rank));
        }
    }

    static class FindResult {
        DS ds;
        int root;
        FindResult(DS ds, int root) {
            this.ds = ds;
            this.root = root;
        }
        FindResult() {}
        @Override public String toString() {
            return String.format("{'ds': %s, 'root': %s}", String.valueOf(ds), String.valueOf(root));
        }
    }

    static DS ds = null;
    static int i = 0;

    static DS make_set(DS ds, int x) {
        int[] p = ((int[])(ds.parent));
        int[] r = ((int[])(ds.rank));
p[x] = x;
r[x] = 0;
        return new DS(p, r);
    }

    static FindResult find_set(DS ds, int x) {
        if (ds.parent[x] == x) {
            return new FindResult(ds, x);
        }
        FindResult res = find_set(ds, ds.parent[x]);
        int[] p_1 = ((int[])(res.ds.parent));
p_1[x] = res.root;
        return new FindResult(new DS(p_1, res.ds.rank), res.root);
    }

    static DS union_set(DS ds, int x, int y) {
        FindResult fx = find_set(ds, x);
        DS ds1 = fx.ds;
        int x_root = fx.root;
        FindResult fy = find_set(ds1, y);
        DS ds2 = fy.ds;
        int y_root = fy.root;
        if (x_root == y_root) {
            return ds2;
        }
        int[] p_2 = ((int[])(ds2.parent));
        int[] r_1 = ((int[])(ds2.rank));
        if (r_1[x_root] > r_1[y_root]) {
p_2[y_root] = x_root;
        } else {
p_2[x_root] = y_root;
            if (r_1[x_root] == r_1[y_root]) {
r_1[y_root] = r_1[y_root] + 1;
            }
        }
        return new DS(p_2, r_1);
    }

    static boolean same_python_set(int a, int b) {
        if (a < 3 && b < 3) {
            return true;
        }
        if (a >= 3 && a < 6 && b >= 3 && b < 6) {
            return true;
        }
        return false;
    }
    public static void main(String[] args) {
        ds = new DS(new int[]{}, new int[]{});
        i = 0;
        while (i < 6) {
ds.parent = java.util.stream.IntStream.concat(java.util.Arrays.stream(ds.parent), java.util.stream.IntStream.of(0)).toArray();
ds.rank = java.util.stream.IntStream.concat(java.util.Arrays.stream(ds.rank), java.util.stream.IntStream.of(0)).toArray();
            ds = make_set(ds, i);
            i = i + 1;
        }
        ds = union_set(ds, 0, 1);
        ds = union_set(ds, 1, 2);
        ds = union_set(ds, 3, 4);
        ds = union_set(ds, 3, 5);
        i = 0;
        while (i < 6) {
            int j = 0;
            while (j < 6) {
                FindResult res_i = find_set(ds, i);
                ds = res_i.ds;
                int root_i = res_i.root;
                FindResult res_j = find_set(ds, j);
                ds = res_j.ds;
                int root_j = res_j.root;
                boolean same = same_python_set(i, j);
                boolean root_same = root_i == root_j;
                if (((Boolean)(same))) {
                    if (!root_same) {
                        throw new RuntimeException(String.valueOf("nodes should be in same set"));
                    }
                } else                 if (root_same) {
                    throw new RuntimeException(String.valueOf("nodes should be in different sets"));
                }
                j = j + 1;
            }
            i = i + 1;
        }
        i = 0;
        while (i < 6) {
            FindResult res_1 = find_set(ds, i);
            ds = res_1.ds;
            System.out.println(_p(res_1.root));
            i = i + 1;
        }
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
