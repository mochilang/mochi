public class Main {
    static java.util.Map<Long,long[]> tree = null;

    static long[] dfs(long start, java.util.Map<Long,Boolean> visited) {
        long size = 1L;
        long cuts_1 = 0L;
visited.put(start, true);
        for (long v : ((long[])(tree).get(start))) {
            if (!(visited.containsKey(v))) {
                long[] res_1 = ((long[])(dfs((long)(v), visited)));
                size = (long)((long)(size) + (long)(res_1[(int)((long)(0))]));
                cuts_1 = (long)((long)(cuts_1) + (long)(res_1[(int)((long)(1))]));
            }
        }
        if (Math.floorMod(size, 2) == 0L) {
            cuts_1 = (long)((long)(cuts_1) + 1L);
        }
        return new long[]{size, cuts_1};
    }

    static long even_tree() {
        java.util.Map<Long,Boolean> visited = ((java.util.Map<Long,Boolean>)(new java.util.LinkedHashMap<Long, Boolean>()));
        long[] res_3 = ((long[])(dfs(1L, visited)));
        return (long)(res_3[(int)((long)(1))]) - 1L;
    }

    static void main() {
        long[][] edges = ((long[][])(new long[][]{new long[]{2, 1}, new long[]{3, 1}, new long[]{4, 3}, new long[]{5, 2}, new long[]{6, 1}, new long[]{7, 2}, new long[]{8, 6}, new long[]{9, 8}, new long[]{10, 8}}));
        long i_1 = 0L;
        while ((long)(i_1) < (long)(edges.length)) {
            long u_1 = (long)(edges[(int)((long)(i_1))][(int)((long)(0))]);
            long v_1 = (long)(edges[(int)((long)(i_1))][(int)((long)(1))]);
            if (!(tree.containsKey(u_1))) {
tree.put(u_1, ((long[])(new long[]{})));
            }
            if (!(tree.containsKey(v_1))) {
tree.put(v_1, ((long[])(new long[]{})));
            }
tree.put(u_1, ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(((long[])(tree).get(u_1))), java.util.stream.LongStream.of((long)(v_1))).toArray())));
tree.put(v_1, ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(((long[])(tree).get(v_1))), java.util.stream.LongStream.of((long)(u_1))).toArray())));
            i_1 = (long)((long)(i_1) + 1L);
        }
        System.out.println(_p(even_tree()));
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            tree = ((java.util.Map<Long,long[]>)(new java.util.LinkedHashMap<Long, long[]>()));
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
