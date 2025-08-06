public class Main {
    static int[][] grid;
    static int[][] delta;
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

    static String key(int y, int x) {
        return _p(y) + "," + _p(x);
    }

    static int parse_int(String s) {
        int value = 0;
        int i = 0;
        while (i < _runeLen(s)) {
            String c = s.substring(i, i+1);
            value = value * 10 + (Integer.parseInt(c));
            i = i + 1;
        }
        return value;
    }

    static int[] parse_key(String k) {
        int idx = 0;
        while (idx < _runeLen(k) && !(_substr(k, idx, idx + 1).equals(","))) {
            idx = idx + 1;
        }
        int y = parse_int(_substr(k, 0, idx));
        int x = parse_int(_substr(k, idx + 1, _runeLen(k)));
        return new int[]{y, x};
    }

    static String[] neighbors(String pos) {
        int[] coords = ((int[])(parse_key(pos)));
        int y_1 = coords[0];
        int x_1 = coords[1];
        String[] res = ((String[])(new String[]{}));
        int i_1 = 0;
        while (i_1 < delta.length) {
            int ny = y_1 + delta[i_1][0];
            int nx = x_1 + delta[i_1][1];
            if (ny >= 0 && ny < grid.length && nx >= 0 && nx < grid[0].length) {
                if (grid[ny][nx] == 0) {
                    res = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(res), java.util.stream.Stream.of(key(ny, nx))).toArray(String[]::new)));
                }
            }
            i_1 = i_1 + 1;
        }
        return res;
    }

    static String[] reverse_list(String[] lst) {
        String[] res_1 = ((String[])(new String[]{}));
        int i_2 = lst.length - 1;
        while (i_2 >= 0) {
            res_1 = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(res_1), java.util.stream.Stream.of(lst[i_2])).toArray(String[]::new)));
            i_2 = i_2 - 1;
        }
        return res_1;
    }

    static String[] bfs(String start, String goal) {
        Node[] queue = ((Node[])(new Node[]{}));
        queue = ((Node[])(java.util.stream.Stream.concat(java.util.Arrays.stream(queue), java.util.stream.Stream.of(new Node(start, new String[]{start}))).toArray(Node[]::new)));
        int head = 0;
        java.util.Map<String,Boolean> visited = ((java.util.Map<String,Boolean>)(new java.util.LinkedHashMap<String, Boolean>(java.util.Map.ofEntries(java.util.Map.entry(start, true)))));
        while (head < queue.length) {
            Node node = queue[head];
            head = head + 1;
            if ((node.pos.equals(goal))) {
                return node.path;
            }
            String[] neigh = ((String[])(neighbors(node.pos)));
            int i_3 = 0;
            while (i_3 < neigh.length) {
                String npos = neigh[i_3];
                if (!(Boolean)(visited.containsKey(npos))) {
visited.put(npos, true);
                    String[] new_path = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(node.path), java.util.stream.Stream.of(npos)).toArray(String[]::new)));
                    queue = ((Node[])(java.util.stream.Stream.concat(java.util.Arrays.stream(queue), java.util.stream.Stream.of(new Node(npos, new_path))).toArray(Node[]::new)));
                }
                i_3 = i_3 + 1;
            }
        }
        return new String[]{};
    }

    static String[] bidirectional_bfs(String start, String goal) {
        Node[] queue_f = ((Node[])(new Node[]{}));
        Node[] queue_b = ((Node[])(new Node[]{}));
        queue_f = ((Node[])(java.util.stream.Stream.concat(java.util.Arrays.stream(queue_f), java.util.stream.Stream.of(new Node(start, new String[]{start}))).toArray(Node[]::new)));
        queue_b = ((Node[])(java.util.stream.Stream.concat(java.util.Arrays.stream(queue_b), java.util.stream.Stream.of(new Node(goal, new String[]{goal}))).toArray(Node[]::new)));
        int head_f = 0;
        int head_b = 0;
        java.util.Map<String,String[]> visited_f = ((java.util.Map<String,String[]>)(new java.util.LinkedHashMap<String, String[]>(java.util.Map.ofEntries(java.util.Map.entry(start, ((String[])(new String[]{start})))))));
        java.util.Map<String,String[]> visited_b = ((java.util.Map<String,String[]>)(new java.util.LinkedHashMap<String, String[]>(java.util.Map.ofEntries(java.util.Map.entry(goal, ((String[])(new String[]{goal})))))));
        while (head_f < queue_f.length && head_b < queue_b.length) {
            Node node_f = queue_f[head_f];
            head_f = head_f + 1;
            String[] neigh_f = ((String[])(neighbors(node_f.pos)));
            int i_4 = 0;
            while (i_4 < neigh_f.length) {
                String npos_1 = neigh_f[i_4];
                if (!(Boolean)(visited_f.containsKey(npos_1))) {
                    String[] new_path_1 = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(node_f.path), java.util.stream.Stream.of(npos_1)).toArray(String[]::new)));
visited_f.put(npos_1, ((String[])(new_path_1)));
                    if (((Boolean)(visited_b.containsKey(npos_1)))) {
                        String[] rev = ((String[])(reverse_list((String[])(((String[])(visited_b).get(npos_1))))));
                        int j = 1;
                        while (j < rev.length) {
                            new_path_1 = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(new_path_1), java.util.stream.Stream.of(rev[j])).toArray(String[]::new)));
                            j = j + 1;
                        }
                        return new_path_1;
                    }
                    queue_f = ((Node[])(java.util.stream.Stream.concat(java.util.Arrays.stream(queue_f), java.util.stream.Stream.of(new Node(npos_1, new_path_1))).toArray(Node[]::new)));
                }
                i_4 = i_4 + 1;
            }
            Node node_b = queue_b[head_b];
            head_b = head_b + 1;
            String[] neigh_b = ((String[])(neighbors(node_b.pos)));
            int j_1 = 0;
            while (j_1 < neigh_b.length) {
                String nposb = neigh_b[j_1];
                if (!(Boolean)(visited_b.containsKey(nposb))) {
                    String[] new_path_b = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(node_b.path), java.util.stream.Stream.of(nposb)).toArray(String[]::new)));
visited_b.put(nposb, ((String[])(new_path_b)));
                    if (((Boolean)(visited_f.containsKey(nposb)))) {
                        String[] path_f = (String[])(((String[])(visited_f).get(nposb)));
                        new_path_b = ((String[])(reverse_list(((String[])(new_path_b)))));
                        int t = 1;
                        while (t < new_path_b.length) {
                            path_f = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(path_f), java.util.stream.Stream.of(new_path_b[t])).toArray(String[]::new)));
                            t = t + 1;
                        }
                        return path_f;
                    }
                    queue_b = ((Node[])(java.util.stream.Stream.concat(java.util.Arrays.stream(queue_b), java.util.stream.Stream.of(new Node(nposb, new_path_b))).toArray(Node[]::new)));
                }
                j_1 = j_1 + 1;
            }
        }
        return new String[]{start};
    }

    static String path_to_string(String[] path) {
        if (path.length == 0) {
            return "[]";
        }
        int[] first = ((int[])(parse_key(path[0])));
        String s = "[(" + _p(_geti(first, 0)) + ", " + _p(_geti(first, 1)) + ")";
        int i_5 = 1;
        while (i_5 < path.length) {
            int[] c_1 = ((int[])(parse_key(path[i_5])));
            s = s + ", (" + _p(_geti(c_1, 0)) + ", " + _p(_geti(c_1, 1)) + ")";
            i_5 = i_5 + 1;
        }
        s = s + "]";
        return s;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            grid = ((int[][])(new int[][]{new int[]{0, 0, 0, 0, 0, 0, 0}, new int[]{0, 1, 0, 0, 0, 0, 0}, new int[]{0, 0, 0, 0, 0, 0, 0}, new int[]{0, 0, 1, 0, 0, 0, 0}, new int[]{1, 0, 1, 0, 0, 0, 0}, new int[]{0, 0, 0, 0, 0, 0, 0}, new int[]{0, 0, 0, 0, 1, 0, 0}}));
            delta = ((int[][])(new int[][]{new int[]{-1, 0}, new int[]{0, -1}, new int[]{1, 0}, new int[]{0, 1}}));
            start = String.valueOf(key(0, 0));
            goal = String.valueOf(key(grid.length - 1, grid[0].length - 1));
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
        return String.valueOf(v);
    }

    static Integer _geti(int[] a, int i) {
        return (i >= 0 && i < a.length) ? a[i] : null;
    }
}
