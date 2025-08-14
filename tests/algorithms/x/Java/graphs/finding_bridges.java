public class Main {
    static class DfsResult {
        long id;
        long[][] bridges;
        DfsResult(long id, long[][] bridges) {
            this.id = id;
            this.bridges = bridges;
        }
        DfsResult() {}
        @Override public String toString() {
            return String.format("{'id': %s, 'bridges': %s}", String.valueOf(id), String.valueOf(bridges));
        }
    }


    static DfsResult dfs(java.util.Map<Long,long[]> graph, long at, long parent, boolean[] visited, long[] ids, long[] low, long id, long[][] bridges) {
visited[(int)((long)(at))] = true;
ids[(int)((long)(at))] = (long)(id);
low[(int)((long)(at))] = (long)(id);
        long current_id_1 = (long)((long)(id) + 1L);
        long[][] res_bridges_1 = ((long[][])(bridges));
        for (long to : ((long[])(graph).get(at))) {
            if ((long)(to) == (long)(parent)) {
                continue;
            } else             if (!(Boolean)visited[(int)((long)(to))]) {
                DfsResult result_1 = dfs(graph, (long)(to), (long)(at), ((boolean[])(visited)), ((long[])(ids)), ((long[])(low)), (long)(current_id_1), ((long[][])(res_bridges_1)));
                current_id_1 = (long)(result_1.id);
                res_bridges_1 = ((long[][])(result_1.bridges));
                if ((long)(low[(int)((long)(at))]) > (long)(low[(int)((long)(to))])) {
low[(int)((long)(at))] = (long)(low[(int)((long)(to))]);
                }
                if ((long)(ids[(int)((long)(at))]) < (long)(low[(int)((long)(to))])) {
                    long[] edge_1 = ((long[])((long)(at) < (long)(to) ? new long[]{at, to} : new long[]{to, at}));
                    res_bridges_1 = ((long[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(res_bridges_1), java.util.stream.Stream.of(new long[][]{edge_1})).toArray(long[][]::new)));
                }
            } else             if ((long)(low[(int)((long)(at))]) > (long)(ids[(int)((long)(to))])) {
low[(int)((long)(at))] = (long)(ids[(int)((long)(to))]);
            }
        }
        return new DfsResult(current_id_1, res_bridges_1);
    }

    static long[][] compute_bridges(java.util.Map<Long,long[]> graph) {
        long n = (long)(graph.size());
        boolean[] visited_1 = ((boolean[])(new boolean[]{}));
        long[] ids_1 = ((long[])(new long[]{}));
        long[] low_1 = ((long[])(new long[]{}));
        long i_1 = 0L;
        while ((long)(i_1) < (long)(n)) {
            visited_1 = ((boolean[])(appendBool(visited_1, false)));
            ids_1 = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(ids_1), java.util.stream.LongStream.of(0L)).toArray()));
            low_1 = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(low_1), java.util.stream.LongStream.of(0L)).toArray()));
            i_1 = (long)((long)(i_1) + 1L);
        }
        long[][] bridges_1 = ((long[][])(new long[][]{}));
        long id_1 = 0L;
        i_1 = 0L;
        while ((long)(i_1) < (long)(n)) {
            if (!(Boolean)visited_1[(int)((long)(i_1))]) {
                DfsResult result_3 = dfs(graph, (long)(i_1), (long)(-1), ((boolean[])(visited_1)), ((long[])(ids_1)), ((long[])(low_1)), (long)(id_1), ((long[][])(bridges_1)));
                id_1 = (long)(result_3.id);
                bridges_1 = ((long[][])(result_3.bridges));
            }
            i_1 = (long)((long)(i_1) + 1L);
        }
        return bridges_1;
    }

    static java.util.Map<Long,long[]> get_demo_graph(long index) {
        if ((long)(index) == 0L) {
            return ((java.util.Map<Long,long[]>)(new java.util.LinkedHashMap<Long, long[]>(java.util.Map.ofEntries(java.util.Map.entry(0L, ((long[])(new long[]{1, 2}))), java.util.Map.entry(1L, ((long[])(new long[]{0, 2}))), java.util.Map.entry(2L, ((long[])(new long[]{0, 1, 3, 5}))), java.util.Map.entry(3L, ((long[])(new long[]{2, 4}))), java.util.Map.entry(4L, ((long[])(new long[]{3}))), java.util.Map.entry(5L, ((long[])(new long[]{2, 6, 8}))), java.util.Map.entry(6L, ((long[])(new long[]{5, 7}))), java.util.Map.entry(7L, ((long[])(new long[]{6, 8}))), java.util.Map.entry(8L, ((long[])(new long[]{5, 7})))))));
        }
        if ((long)(index) == 1L) {
            return ((java.util.Map<Long,long[]>)(new java.util.LinkedHashMap<Long, long[]>(java.util.Map.ofEntries(java.util.Map.entry(0L, ((long[])(new long[]{6}))), java.util.Map.entry(1L, ((long[])(new long[]{9}))), java.util.Map.entry(2L, ((long[])(new long[]{4, 5}))), java.util.Map.entry(3L, ((long[])(new long[]{4}))), java.util.Map.entry(4L, ((long[])(new long[]{2, 3}))), java.util.Map.entry(5L, ((long[])(new long[]{2}))), java.util.Map.entry(6L, ((long[])(new long[]{0, 7}))), java.util.Map.entry(7L, ((long[])(new long[]{6}))), java.util.Map.entry(8L, ((long[])(new long[]{}))), java.util.Map.entry(9L, ((long[])(new long[]{1})))))));
        }
        if ((long)(index) == 2L) {
            return ((java.util.Map<Long,long[]>)(new java.util.LinkedHashMap<Long, long[]>(java.util.Map.ofEntries(java.util.Map.entry(0L, ((long[])(new long[]{4}))), java.util.Map.entry(1L, ((long[])(new long[]{6}))), java.util.Map.entry(2L, ((long[])(new long[]{}))), java.util.Map.entry(3L, ((long[])(new long[]{5, 6, 7}))), java.util.Map.entry(4L, ((long[])(new long[]{0, 6}))), java.util.Map.entry(5L, ((long[])(new long[]{3, 8, 9}))), java.util.Map.entry(6L, ((long[])(new long[]{1, 3, 4, 7}))), java.util.Map.entry(7L, ((long[])(new long[]{3, 6, 8, 9}))), java.util.Map.entry(8L, ((long[])(new long[]{5, 7}))), java.util.Map.entry(9L, ((long[])(new long[]{5, 7})))))));
        }
        return ((java.util.Map<Long,long[]>)(new java.util.LinkedHashMap<Long, long[]>(java.util.Map.ofEntries(java.util.Map.entry(0L, ((long[])(new long[]{1, 3}))), java.util.Map.entry(1L, ((long[])(new long[]{0, 2, 4}))), java.util.Map.entry(2L, ((long[])(new long[]{1, 3, 4}))), java.util.Map.entry(3L, ((long[])(new long[]{0, 2, 4}))), java.util.Map.entry(4L, ((long[])(new long[]{1, 2, 3})))))));
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            System.out.println(compute_bridges(get_demo_graph(0L)));
            System.out.println(compute_bridges(get_demo_graph(1L)));
            System.out.println(compute_bridges(get_demo_graph(2L)));
            System.out.println(compute_bridges(get_demo_graph(3L)));
            System.out.println(compute_bridges(((java.util.Map<Long,long[]>)(new java.util.LinkedHashMap<Long, long[]>()))));
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

    static boolean[] appendBool(boolean[] arr, boolean v) {
        boolean[] out = java.util.Arrays.copyOf(arr, arr.length + 1);
        out[arr.length] = v;
        return out;
    }
}
