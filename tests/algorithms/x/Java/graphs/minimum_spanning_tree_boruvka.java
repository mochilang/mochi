public class Main {
    static class Edge {
        long u;
        long v;
        long w;
        Edge(long u, long v, long w) {
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
        long[] parent;
        long[] rank;
        UF(long[] parent, long[] rank) {
            this.parent = parent;
            this.rank = rank;
        }
        UF() {}
        @Override public String toString() {
            return String.format("{'parent': %s, 'rank': %s}", String.valueOf(parent), String.valueOf(rank));
        }
    }

    static class FindRes {
        long root;
        UF uf;
        FindRes(long root, UF uf) {
            this.root = root;
            this.uf = uf;
        }
        FindRes() {}
        @Override public String toString() {
            return String.format("{'root': %s, 'uf': %s}", String.valueOf(root), String.valueOf(uf));
        }
    }


    static UF uf_make(long n) {
        long[] p = ((long[])(new long[]{}));
        long[] r_1 = ((long[])(new long[]{}));
        long i_1 = 0L;
        while ((long)(i_1) < (long)(n)) {
            p = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(p), java.util.stream.LongStream.of((long)(i_1))).toArray()));
            r_1 = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(r_1), java.util.stream.LongStream.of(0L)).toArray()));
            i_1 = (long)((long)(i_1) + 1L);
        }
        return new UF(p, r_1);
    }

    static FindRes uf_find(UF uf, long x) {
        long[] p_1 = ((long[])(uf.parent));
        if ((long)(p_1[(int)((long)(x))]) != (long)(x)) {
            FindRes res_1 = uf_find(new UF(p_1, uf.rank), (long)(p_1[(int)((long)(x))]));
            p_1 = ((long[])(res_1.uf.parent));
p_1[(int)((long)(x))] = (long)(res_1.root);
            return new FindRes(res_1.root, new UF(p_1, res_1.uf.rank));
        }
        return new FindRes(x, uf);
    }

    static UF uf_union(UF uf, long x, long y) {
        FindRes fr1 = uf_find(uf, (long)(x));
        UF uf1_1 = fr1.uf;
        long root1_1 = (long)(fr1.root);
        FindRes fr2_1 = uf_find(uf1_1, (long)(y));
        uf1_1 = fr2_1.uf;
        long root2_1 = (long)(fr2_1.root);
        if ((long)(root1_1) == (long)(root2_1)) {
            return uf1_1;
        }
        long[] p_3 = ((long[])(uf1_1.parent));
        long[] r_3 = ((long[])(uf1_1.rank));
        if ((long)(r_3[(int)((long)(root1_1))]) > (long)(r_3[(int)((long)(root2_1))])) {
p_3[(int)((long)(root2_1))] = (long)(root1_1);
        } else         if ((long)(r_3[(int)((long)(root1_1))]) < (long)(r_3[(int)((long)(root2_1))])) {
p_3[(int)((long)(root1_1))] = (long)(root2_1);
        } else {
p_3[(int)((long)(root2_1))] = (long)(root1_1);
r_3[(int)((long)(root1_1))] = (long)((long)(r_3[(int)((long)(root1_1))]) + 1L);
        }
        return new UF(p_3, r_3);
    }

    static Edge[] boruvka(long n, Edge[] edges) {
        UF uf = uf_make((long)(n));
        long num_components_1 = (long)(n);
        Edge[] mst_1 = ((Edge[])(new Edge[]{}));
        while ((long)(num_components_1) > 1L) {
            long[] cheap_1 = ((long[])(new long[]{}));
            long i_3 = 0L;
            while ((long)(i_3) < (long)(n)) {
                cheap_1 = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(cheap_1), java.util.stream.LongStream.of((long)(0L - 1L))).toArray()));
                i_3 = (long)((long)(i_3) + 1L);
            }
            long idx_1 = 0L;
            while ((long)(idx_1) < (long)(edges.length)) {
                Edge e_2 = edges[(int)((long)(idx_1))];
                FindRes fr1_3 = uf_find(uf, (long)(e_2.u));
                uf = fr1_3.uf;
                long set1_2 = (long)(fr1_3.root);
                FindRes fr2_4 = uf_find(uf, (long)(e_2.v));
                uf = fr2_4.uf;
                long set2_2 = (long)(fr2_4.root);
                if ((long)(set1_2) != (long)(set2_2)) {
                    if ((long)(cheap_1[(int)((long)(set1_2))]) == (long)(0L - 1L) || (long)(edges[(int)((long)(cheap_1[(int)((long)(set1_2))]))].w) > (long)(e_2.w)) {
cheap_1[(int)((long)(set1_2))] = (long)(idx_1);
                    }
                    if ((long)(cheap_1[(int)((long)(set2_2))]) == (long)(0L - 1L) || (long)(edges[(int)((long)(cheap_1[(int)((long)(set2_2))]))].w) > (long)(e_2.w)) {
cheap_1[(int)((long)(set2_2))] = (long)(idx_1);
                    }
                }
                idx_1 = (long)((long)(idx_1) + 1L);
            }
            long v_1 = 0L;
            while ((long)(v_1) < (long)(n)) {
                long idxe_1 = (long)(cheap_1[(int)((long)(v_1))]);
                if ((long)(idxe_1) != (long)(0L - 1L)) {
                    Edge e_3 = edges[(int)((long)(idxe_1))];
                    FindRes fr1_4 = uf_find(uf, (long)(e_3.u));
                    uf = fr1_4.uf;
                    long set1_3 = (long)(fr1_4.root);
                    FindRes fr2_5 = uf_find(uf, (long)(e_3.v));
                    uf = fr2_5.uf;
                    long set2_3 = (long)(fr2_5.root);
                    if ((long)(set1_3) != (long)(set2_3)) {
                        mst_1 = ((Edge[])(java.util.stream.Stream.concat(java.util.Arrays.stream(mst_1), java.util.stream.Stream.of(e_3)).toArray(Edge[]::new)));
                        uf = uf_union(uf, (long)(set1_3), (long)(set2_3));
                        num_components_1 = (long)((long)(num_components_1) - 1L);
                    }
                }
                v_1 = (long)((long)(v_1) + 1L);
            }
        }
        return mst_1;
    }

    static void main() {
        Edge[] edges = ((Edge[])(new Edge[]{new Edge(0, 1, 1), new Edge(0, 2, 2), new Edge(2, 3, 3)}));
        Edge[] mst_3 = ((Edge[])(boruvka(4L, ((Edge[])(edges)))));
        for (Edge e : mst_3) {
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
        if (v instanceof Double || v instanceof Float) {
            double d = ((Number) v).doubleValue();
            return String.valueOf(d);
        }
        return String.valueOf(v);
    }
}
