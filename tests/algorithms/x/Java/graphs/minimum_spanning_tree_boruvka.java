public class Main {
    static class Edge {
        int u;
        int v;
        int w;
        Edge(int u, int v, int w) {
            this.u = u;
            this.v = v;
            this.w = w;
        }
        Edge() {}
        @Override public String toString() {
            return String.format("{'u': %s, 'v': %s, 'w': %s}", String.valueOf(u), String.valueOf(v), String.valueOf(w));
        }
    }

    static class UF {
        int[] parent;
        int[] rank;
        UF(int[] parent, int[] rank) {
            this.parent = parent;
            this.rank = rank;
        }
        UF() {}
        @Override public String toString() {
            return String.format("{'parent': %s, 'rank': %s}", String.valueOf(parent), String.valueOf(rank));
        }
    }

    static class FindRes {
        int root;
        UF uf;
        FindRes(int root, UF uf) {
            this.root = root;
            this.uf = uf;
        }
        FindRes() {}
        @Override public String toString() {
            return String.format("{'root': %s, 'uf': %s}", String.valueOf(root), String.valueOf(uf));
        }
    }


    static UF uf_make(int n) {
        int[] p = ((int[])(new int[]{}));
        int[] r = ((int[])(new int[]{}));
        int i = 0;
        while (i < n) {
            p = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(p), java.util.stream.IntStream.of(i)).toArray()));
            r = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(r), java.util.stream.IntStream.of(0)).toArray()));
            i = i + 1;
        }
        return new UF(p, r);
    }

    static FindRes uf_find(UF uf, int x) {
        int[] p_1 = ((int[])(uf.parent));
        if (p_1[x] != x) {
            FindRes res = uf_find(new UF(p_1, uf.rank), p_1[x]);
            p_1 = ((int[])(res.uf.parent));
p_1[x] = res.root;
            return new FindRes(res.root, new UF(p_1, res.uf.rank));
        }
        return new FindRes(x, uf);
    }

    static UF uf_union(UF uf, int x, int y) {
        FindRes fr1 = uf_find(uf, x);
        UF uf1 = fr1.uf;
        int root1 = fr1.root;
        FindRes fr2 = uf_find(uf1, y);
        uf1 = fr2.uf;
        int root2 = fr2.root;
        if (root1 == root2) {
            return uf1;
        }
        int[] p_2 = ((int[])(uf1.parent));
        int[] r_1 = ((int[])(uf1.rank));
        if (r_1[root1] > r_1[root2]) {
p_2[root2] = root1;
        } else         if (r_1[root1] < r_1[root2]) {
p_2[root1] = root2;
        } else {
p_2[root2] = root1;
r_1[root1] = r_1[root1] + 1;
        }
        return new UF(p_2, r_1);
    }

    static Edge[] boruvka(int n, Edge[] edges) {
        UF uf = uf_make(n);
        int num_components = n;
        Edge[] mst = ((Edge[])(new Edge[]{}));
        while (num_components > 1) {
            int[] cheap = ((int[])(new int[]{}));
            int i_1 = 0;
            while (i_1 < n) {
                cheap = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(cheap), java.util.stream.IntStream.of(0 - 1)).toArray()));
                i_1 = i_1 + 1;
            }
            int idx = 0;
            while (idx < edges.length) {
                Edge e = edges[idx];
                FindRes fr1_1 = uf_find(uf, e.u);
                uf = fr1_1.uf;
                int set1 = fr1_1.root;
                FindRes fr2_1 = uf_find(uf, e.v);
                uf = fr2_1.uf;
                int set2 = fr2_1.root;
                if (set1 != set2) {
                    if (cheap[set1] == 0 - 1 || edges[cheap[set1]].w > e.w) {
cheap[set1] = idx;
                    }
                    if (cheap[set2] == 0 - 1 || edges[cheap[set2]].w > e.w) {
cheap[set2] = idx;
                    }
                }
                idx = idx + 1;
            }
            int v = 0;
            while (v < n) {
                int idxe = cheap[v];
                if (idxe != 0 - 1) {
                    Edge e_1 = edges[idxe];
                    FindRes fr1_2 = uf_find(uf, e_1.u);
                    uf = fr1_2.uf;
                    int set1_1 = fr1_2.root;
                    FindRes fr2_2 = uf_find(uf, e_1.v);
                    uf = fr2_2.uf;
                    int set2_1 = fr2_2.root;
                    if (set1_1 != set2_1) {
                        mst = ((Edge[])(java.util.stream.Stream.concat(java.util.Arrays.stream(mst), java.util.stream.Stream.of(e_1)).toArray(Edge[]::new)));
                        uf = uf_union(uf, set1_1, set2_1);
                        num_components = num_components - 1;
                    }
                }
                v = v + 1;
            }
        }
        return mst;
    }

    static void main() {
        Edge[] edges = ((Edge[])(new Edge[]{new Edge(0, 1, 1), new Edge(0, 2, 2), new Edge(2, 3, 3)}));
        Edge[] mst_1 = ((Edge[])(boruvka(4, ((Edge[])(edges)))));
        for (Edge e : mst_1) {
            System.out.println(_p(e.u) + " - " + _p(e.v) + " : " + _p(e.w));
        }
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
