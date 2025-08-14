public class Main {
    static long[][] grid;
    static long[][] delta;
    static class Node {
        String pos;
        String[] path;
        Node(String pos, String[] path) {
            this.pos = pos;
            this.path = path;
        }
        Node() {}
        @Override public String toString() {
            return String.format("{'pos': '%s', 'path': %s}", String.valueOf(pos), String.valueOf(path));
        }
    }

    static String start;
    static String goal;
    static String[] path1;
    static String[] path2;

    static String key(long y, long x) {
        return _p(y) + "," + _p(x);
    }

    static long parse_int(String s) {
        long value = 0L;
        long i_1 = 0L;
        while ((long)(i_1) < (long)(_runeLen(s))) {
            String c_1 = s.substring((int)((long)(i_1)), (int)((long)(i_1))+1);
            value = (long)((long)((long)(value) * 10L) + (long)((Integer.parseInt(c_1))));
            i_1 = (long)((long)(i_1) + 1L);
        }
        return value;
    }

    static long[] parse_key(String k) {
        long idx = 0L;
        while ((long)(idx) < (long)(_runeLen(k)) && !(_substr(k, (int)((long)(idx)), (int)((long)((long)(idx) + 1L))).equals(","))) {
            idx = (long)((long)(idx) + 1L);
        }
        long y_1 = (long)(parse_int(_substr(k, (int)((long)(0)), (int)((long)(idx)))));
        long x_1 = (long)(parse_int(_substr(k, (int)((long)((long)(idx) + 1L)), (int)((long)(_runeLen(k))))));
        return new long[]{y_1, x_1};
    }

    static String[] neighbors(String pos) {
        long[] coords = ((long[])(parse_key(pos)));
        long y_3 = (long)(coords[(int)((long)(0))]);
        long x_3 = (long)(coords[(int)((long)(1))]);
        String[] res_1 = ((String[])(new String[]{}));
        long i_3 = 0L;
        while ((long)(i_3) < (long)(delta.length)) {
            long ny_1 = (long)((long)(y_3) + (long)(delta[(int)((long)(i_3))][(int)((long)(0))]));
            long nx_1 = (long)((long)(x_3) + (long)(delta[(int)((long)(i_3))][(int)((long)(1))]));
            if ((long)(ny_1) >= 0L && (long)(ny_1) < (long)(grid.length) && (long)(nx_1) >= 0L && (long)(nx_1) < (long)(grid[(int)((long)(0))].length)) {
                if ((long)(grid[(int)((long)(ny_1))][(int)((long)(nx_1))]) == 0L) {
                    res_1 = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(res_1), java.util.stream.Stream.of(key((long)(ny_1), (long)(nx_1)))).toArray(String[]::new)));
                }
            }
            i_3 = (long)((long)(i_3) + 1L);
        }
        return res_1;
    }

    static String[] reverse_list(String[] lst) {
        String[] res_2 = ((String[])(new String[]{}));
        long i_5 = (long)((long)(lst.length) - 1L);
        while ((long)(i_5) >= 0L) {
            res_2 = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(res_2), java.util.stream.Stream.of(lst[(int)((long)(i_5))])).toArray(String[]::new)));
            i_5 = (long)((long)(i_5) - 1L);
        }
        return res_2;
    }

    static String[] bfs(String start, String goal) {
        Node[] queue = ((Node[])(new Node[]{}));
        queue = ((Node[])(java.util.stream.Stream.concat(java.util.Arrays.stream(queue), java.util.stream.Stream.of(new Node(start, new String[]{start}))).toArray(Node[]::new)));
        long head_1 = 0L;
        java.util.Map<String,Boolean> visited_1 = ((java.util.Map<String,Boolean>)(new java.util.LinkedHashMap<String, Boolean>(java.util.Map.ofEntries(java.util.Map.entry(start, true)))));
        while ((long)(head_1) < (long)(queue.length)) {
            Node node_1 = queue[(int)((long)(head_1))];
            head_1 = (long)((long)(head_1) + 1L);
            if ((node_1.pos.equals(goal))) {
                return node_1.path;
            }
            String[] neigh_1 = ((String[])(neighbors(node_1.pos)));
            long i_7 = 0L;
            while ((long)(i_7) < (long)(neigh_1.length)) {
                String npos_1 = neigh_1[(int)((long)(i_7))];
                if (!(visited_1.containsKey(npos_1))) {
visited_1.put(npos_1, true);
                    String[] new_path_1 = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(node_1.path), java.util.stream.Stream.of(npos_1)).toArray(String[]::new)));
                    queue = ((Node[])(java.util.stream.Stream.concat(java.util.Arrays.stream(queue), java.util.stream.Stream.of(new Node(npos_1, new_path_1))).toArray(Node[]::new)));
                }
                i_7 = (long)((long)(i_7) + 1L);
            }
        }
        return new String[]{};
    }

    static String[] bidirectional_bfs(String start, String goal) {
        Node[] queue_f = ((Node[])(new Node[]{}));
        Node[] queue_b_1 = ((Node[])(new Node[]{}));
        queue_f = ((Node[])(java.util.stream.Stream.concat(java.util.Arrays.stream(queue_f), java.util.stream.Stream.of(new Node(start, new String[]{start}))).toArray(Node[]::new)));
        queue_b_1 = ((Node[])(java.util.stream.Stream.concat(java.util.Arrays.stream(queue_b_1), java.util.stream.Stream.of(new Node(goal, new String[]{goal}))).toArray(Node[]::new)));
        long head_f_1 = 0L;
        long head_b_1 = 0L;
        java.util.Map<String,String[]> visited_f_1 = ((java.util.Map<String,String[]>)(new java.util.LinkedHashMap<String, String[]>(java.util.Map.ofEntries(java.util.Map.entry(start, ((String[])(new String[]{start})))))));
        java.util.Map<String,String[]> visited_b_1 = ((java.util.Map<String,String[]>)(new java.util.LinkedHashMap<String, String[]>(java.util.Map.ofEntries(java.util.Map.entry(goal, ((String[])(new String[]{goal})))))));
        while ((long)(head_f_1) < (long)(queue_f.length) && (long)(head_b_1) < (long)(queue_b_1.length)) {
            Node node_f_1 = queue_f[(int)((long)(head_f_1))];
            head_f_1 = (long)((long)(head_f_1) + 1L);
            String[] neigh_f_1 = ((String[])(neighbors(node_f_1.pos)));
            long i_9 = 0L;
            while ((long)(i_9) < (long)(neigh_f_1.length)) {
                String npos_3 = neigh_f_1[(int)((long)(i_9))];
                if (!(visited_f_1.containsKey(npos_3))) {
                    String[] new_path_3 = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(node_f_1.path), java.util.stream.Stream.of(npos_3)).toArray(String[]::new)));
visited_f_1.put(npos_3, ((String[])(new_path_3)));
                    if (visited_b_1.containsKey(npos_3)) {
                        String[] rev_1 = ((String[])(reverse_list((String[])(((String[])(visited_b_1).get(npos_3))))));
                        long j_2 = 1L;
                        while ((long)(j_2) < (long)(rev_1.length)) {
                            new_path_3 = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(new_path_3), java.util.stream.Stream.of(rev_1[(int)((long)(j_2))])).toArray(String[]::new)));
                            j_2 = (long)((long)(j_2) + 1L);
                        }
                        return new_path_3;
                    }
                    queue_f = ((Node[])(java.util.stream.Stream.concat(java.util.Arrays.stream(queue_f), java.util.stream.Stream.of(new Node(npos_3, new_path_3))).toArray(Node[]::new)));
                }
                i_9 = (long)((long)(i_9) + 1L);
            }
            Node node_b_1 = queue_b_1[(int)((long)(head_b_1))];
            head_b_1 = (long)((long)(head_b_1) + 1L);
            String[] neigh_b_1 = ((String[])(neighbors(node_b_1.pos)));
            long j_3 = 0L;
            while ((long)(j_3) < (long)(neigh_b_1.length)) {
                String nposb_1 = neigh_b_1[(int)((long)(j_3))];
                if (!(visited_b_1.containsKey(nposb_1))) {
                    String[] new_path_b_1 = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(node_b_1.path), java.util.stream.Stream.of(nposb_1)).toArray(String[]::new)));
visited_b_1.put(nposb_1, ((String[])(new_path_b_1)));
                    if (visited_f_1.containsKey(nposb_1)) {
                        String[] path_f_1 = (String[])(((String[])(visited_f_1).get(nposb_1)));
                        new_path_b_1 = ((String[])(reverse_list(((String[])(new_path_b_1)))));
                        long t_1 = 1L;
                        while ((long)(t_1) < (long)(new_path_b_1.length)) {
                            path_f_1 = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(path_f_1), java.util.stream.Stream.of(new_path_b_1[(int)((long)(t_1))])).toArray(String[]::new)));
                            t_1 = (long)((long)(t_1) + 1L);
                        }
                        return path_f_1;
                    }
                    queue_b_1 = ((Node[])(java.util.stream.Stream.concat(java.util.Arrays.stream(queue_b_1), java.util.stream.Stream.of(new Node(nposb_1, new_path_b_1))).toArray(Node[]::new)));
                }
                j_3 = (long)((long)(j_3) + 1L);
            }
        }
        return new String[]{start};
    }

    static String path_to_string(String[] path) {
        if ((long)(path.length) == 0L) {
            return "[]";
        }
        long[] first_1 = ((long[])(parse_key(path[(int)((long)(0))])));
        String s_1 = "[(" + _p(_geti(first_1, ((Number)(0)).intValue())) + ", " + _p(_geti(first_1, ((Number)(1)).intValue())) + ")";
        long i_11 = 1L;
        while ((long)(i_11) < (long)(path.length)) {
            long[] c_3 = ((long[])(parse_key(path[(int)((long)(i_11))])));
            s_1 = s_1 + ", (" + _p(_geti(c_3, ((Number)(0)).intValue())) + ", " + _p(_geti(c_3, ((Number)(1)).intValue())) + ")";
            i_11 = (long)((long)(i_11) + 1L);
        }
        s_1 = s_1 + "]";
        return s_1;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            grid = ((long[][])(new long[][]{new long[]{0, 0, 0, 0, 0, 0, 0}, new long[]{0, 1, 0, 0, 0, 0, 0}, new long[]{0, 0, 0, 0, 0, 0, 0}, new long[]{0, 0, 1, 0, 0, 0, 0}, new long[]{1, 0, 1, 0, 0, 0, 0}, new long[]{0, 0, 0, 0, 0, 0, 0}, new long[]{0, 0, 0, 0, 1, 0, 0}}));
            delta = ((long[][])(new long[][]{new long[]{-1, 0}, new long[]{0, -1}, new long[]{1, 0}, new long[]{0, 1}}));
            start = String.valueOf(key(0L, 0L));
            goal = String.valueOf(key((long)((long)(grid.length) - 1L), (long)((long)(grid[(int)((long)(0))].length) - 1L)));
            path1 = ((String[])(bfs(start, goal)));
            System.out.println(path_to_string(((String[])(path1))));
            path2 = ((String[])(bidirectional_bfs(start, goal)));
            System.out.println(path_to_string(((String[])(path2))));
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

    static int _runeLen(String s) {
        return s.codePointCount(0, s.length());
    }

    static String _substr(String s, int i, int j) {
        int len = _runeLen(s);
        if (i < 0) i = 0;
        if (j > len) j = len;
        if (i > j) i = j;
        int start = s.offsetByCodePoints(0, i);
        int end = s.offsetByCodePoints(0, j);
        return s.substring(start, end);
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
