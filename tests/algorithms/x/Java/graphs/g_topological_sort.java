public class Main {

    static long[] depth_first_search(long u, boolean[] visited, long[][] graph, long[] stack) {
visited[(int)((long)(u))] = true;
        long i_1 = 0L;
        while ((long)(i_1) < (long)(graph[(int)((long)(u))].length)) {
            long v_1 = (long)(graph[(int)((long)(u))][(int)((long)(i_1))]);
            if (!(Boolean)visited[(int)((long)(v_1))]) {
                stack = ((long[])(depth_first_search((long)(v_1), ((boolean[])(visited)), ((long[][])(graph)), ((long[])(stack)))));
            }
            i_1 = (long)((long)(i_1) + 1L);
        }
        stack = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(stack), java.util.stream.LongStream.of((long)(u))).toArray()));
        return stack;
    }

    static long[] topological_sort(long[][] graph) {
        boolean[] visited = ((boolean[])(new boolean[]{}));
        long i_3 = 0L;
        while ((long)(i_3) < (long)(graph.length)) {
            visited = ((boolean[])(appendBool(visited, false)));
            i_3 = (long)((long)(i_3) + 1L);
        }
        long[] stack_1 = ((long[])(new long[]{}));
        i_3 = 0L;
        while ((long)(i_3) < (long)(graph.length)) {
            if (!(Boolean)visited[(int)((long)(i_3))]) {
                stack_1 = ((long[])(depth_first_search((long)(i_3), ((boolean[])(visited)), ((long[][])(graph)), ((long[])(stack_1)))));
            }
            i_3 = (long)((long)(i_3) + 1L);
        }
        return stack_1;
    }

    static void print_stack(long[] stack, java.util.Map<Long,String> clothes) {
        long order = 1L;
        long[] s_1 = ((long[])(stack));
        while ((long)(s_1.length) > 0L) {
            long idx_1 = (long)(s_1[(int)((long)((long)(s_1.length) - 1L))]);
            s_1 = ((long[])(java.util.Arrays.copyOfRange(s_1, (int)((long)(0)), (int)((long)((long)(s_1.length) - 1L)))));
            System.out.println(_p(order) + " " + ((String)(clothes).get(idx_1)));
            order = (long)((long)(order) + 1L);
        }
    }

    static String format_list(long[] xs) {
        String res = "[";
        long i_5 = 0L;
        while ((long)(i_5) < (long)(xs.length)) {
            res = res + _p(_geti(xs, ((Number)(i_5)).intValue()));
            if ((long)(i_5) < (long)((long)(xs.length) - 1L)) {
                res = res + ", ";
            }
            i_5 = (long)((long)(i_5) + 1L);
        }
        res = res + "]";
        return res;
    }

    static void main() {
        java.util.Map<Long,String> clothes = ((java.util.Map<Long,String>)(new java.util.LinkedHashMap<Long, String>(java.util.Map.ofEntries(java.util.Map.entry(0L, "underwear"), java.util.Map.entry(1L, "pants"), java.util.Map.entry(2L, "belt"), java.util.Map.entry(3L, "suit"), java.util.Map.entry(4L, "shoe"), java.util.Map.entry(5L, "socks"), java.util.Map.entry(6L, "shirt"), java.util.Map.entry(7L, "tie"), java.util.Map.entry(8L, "watch")))));
        long[][] graph_1 = ((long[][])(new long[][]{new long[]{1, 4}, new long[]{2, 4}, new long[]{3}, new long[]{}, new long[]{}, new long[]{4}, new long[]{2, 7}, new long[]{3}, new long[]{}}));
        long[] stack_3 = ((long[])(topological_sort(((long[][])(graph_1)))));
        System.out.println(format_list(((long[])(stack_3))));
        print_stack(((long[])(stack_3)), clothes);
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
        if (v instanceof Double || v instanceof Float) {
            double d = ((Number) v).doubleValue();
            return String.valueOf(d);
        }
        return String.valueOf(v);
    }

    static Long _geti(long[] a, int i) {
        return (i >= 0 && i < a.length) ? a[i] : null;
    }
}
