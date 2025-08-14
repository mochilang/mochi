public class Main {
    static java.util.Map<Long,long[]> graph = null;

    static long[] remove_value(long[] lst, long val) {
        long[] res = ((long[])(new long[]{}));
        long i_1 = 0L;
        while ((long)(i_1) < (long)(lst.length)) {
            if ((long)(lst[(int)((long)(i_1))]) != (long)(val)) {
                res = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(res), java.util.stream.LongStream.of((long)(lst[(int)((long)(i_1))]))).toArray()));
            }
            i_1 = (long)((long)(i_1) + 1L);
        }
        return res;
    }

    static long[] greedy_min_vertex_cover(java.util.Map<Long,long[]> graph) {
        java.util.Map<Long,long[]> g = graph;
        long[] cover_1 = ((long[])(new long[]{}));
        while (true) {
            long max_v_1 = 0L;
            long max_deg_1 = 0L;
            for (long v : g.keySet()) {
                long key_1 = (long)(((Number)(v)).intValue());
                long deg_1 = (long)(((long[])(g).get(key_1)).length);
                if ((long)(deg_1) > (long)(max_deg_1)) {
                    max_deg_1 = (long)(deg_1);
                    max_v_1 = (long)(key_1);
                }
            }
            if ((long)(max_deg_1) == 0L) {
                break;
            }
            cover_1 = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(cover_1), java.util.stream.LongStream.of((long)(max_v_1))).toArray()));
            long[] neighbors_1 = (long[])(((long[])(g).get(max_v_1)));
            long i_3 = 0L;
            while ((long)(i_3) < (long)(neighbors_1.length)) {
                long n_1 = (long)(neighbors_1[(int)((long)(i_3))]);
g.put(n_1, ((long[])(remove_value((long[])(((long[])(g).get(n_1))), (long)(max_v_1)))));
                i_3 = (long)((long)(i_3) + 1L);
            }
g.put(max_v_1, ((long[])(new long[]{})));
        }
        return cover_1;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            graph = ((java.util.Map<Long,long[]>)(new java.util.LinkedHashMap<Long, long[]>(java.util.Map.ofEntries(java.util.Map.entry(0L, ((long[])(new long[]{1, 3}))), java.util.Map.entry(1L, ((long[])(new long[]{0, 3}))), java.util.Map.entry(2L, ((long[])(new long[]{0, 3, 4}))), java.util.Map.entry(3L, ((long[])(new long[]{0, 1, 2}))), java.util.Map.entry(4L, ((long[])(new long[]{2, 3})))))));
            System.out.println(greedy_min_vertex_cover(graph));
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
}
