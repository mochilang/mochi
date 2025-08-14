public class Main {
    static long[][] g1;
    static long[][] g2;

    static boolean depth_first_search(long[][] graph, long vertex, boolean[] visited, boolean[] rec_stk) {
visited[(int)((long)(vertex))] = true;
rec_stk[(int)((long)(vertex))] = true;
        for (long node : graph[(int)((long)(vertex))]) {
            if (!(Boolean)visited[(int)((long)(node))]) {
                if (depth_first_search(((long[][])(graph)), (long)(node), ((boolean[])(visited)), ((boolean[])(rec_stk)))) {
                    return true;
                }
            } else             if (rec_stk[(int)((long)(node))]) {
                return true;
            }
        }
rec_stk[(int)((long)(vertex))] = false;
        return false;
    }

    static boolean check_cycle(long[][] graph) {
        long n = (long)(graph.length);
        boolean[] visited_1 = ((boolean[])(new boolean[]{}));
        boolean[] rec_stk_1 = ((boolean[])(new boolean[]{}));
        long i_1 = 0L;
        while ((long)(i_1) < (long)(n)) {
            visited_1 = ((boolean[])(appendBool(visited_1, false)));
            rec_stk_1 = ((boolean[])(appendBool(rec_stk_1, false)));
            i_1 = (long)((long)(i_1) + 1L);
        }
        i_1 = 0L;
        while ((long)(i_1) < (long)(n)) {
            if (!(Boolean)visited_1[(int)((long)(i_1))]) {
                if (depth_first_search(((long[][])(graph)), (long)(i_1), ((boolean[])(visited_1)), ((boolean[])(rec_stk_1)))) {
                    return true;
                }
            }
            i_1 = (long)((long)(i_1) + 1L);
        }
        return false;
    }

    static void print_bool(boolean b) {
        if (b) {
            System.out.println(true ? "True" : "False");
        } else {
            System.out.println(false ? "True" : "False");
        }
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            g1 = ((long[][])(new long[][]{new long[]{}, new long[]{0, 3}, new long[]{0, 4}, new long[]{5}, new long[]{5}, new long[]{}}));
            print_bool(check_cycle(((long[][])(g1))));
            g2 = ((long[][])(new long[][]{new long[]{1, 2}, new long[]{2}, new long[]{0, 3}, new long[]{3}}));
            print_bool(check_cycle(((long[][])(g2))));
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
