public class Main {
    static int HEURISTIC;
    static int[][] grid;
    static int[][] delta;
    static class Pos {
        int y;
        int x;
        Pos(int y, int x) {
            this.y = y;
            this.x = x;
        }
        Pos() {}
        @Override public String toString() {
            return String.format("{'y': %s, 'x': %s}", String.valueOf(y), String.valueOf(x));
        }
    }

    static class Node {
        Pos pos;
        int g_cost;
        double h_cost;
        double f_cost;
        Pos[] path;
        Node(Pos pos, int g_cost, double h_cost, double f_cost, Pos[] path) {
            this.pos = pos;
            this.g_cost = g_cost;
            this.h_cost = h_cost;
            this.f_cost = f_cost;
            this.path = path;
        }
        Node() {}
        @Override public String toString() {
            return String.format("{'pos': %s, 'g_cost': %s, 'h_cost': %s, 'f_cost': %s, 'path': %s}", String.valueOf(pos), String.valueOf(g_cost), String.valueOf(h_cost), String.valueOf(f_cost), String.valueOf(path));
        }
    }

    static Pos start;
    static Pos goal;
    static Pos[] path1;
    static Pos[] path2;

    static int abs(int x) {
        if (x < 0) {
            return -x;
        }
        return x;
    }

    static double sqrtApprox(double x) {
        if (x <= 0.0) {
            return 0.0;
        }
        double guess = x;
        int i = 0;
        while (i < 10) {
            guess = (guess + x / guess) / 2.0;
            i = i + 1;
        }
        return guess;
    }

    static double heuristic(Pos a, Pos b) {
        int dy = a.y - b.y;
        int dx = a.x - b.x;
        if (HEURISTIC == 1) {
            return ((Number)((Math.abs(dy) + Math.abs(dx)))).doubleValue();
        }
        double dyf = (((Number)(dy)).doubleValue());
        double dxf = (((Number)(dx)).doubleValue());
        return sqrtApprox(dyf * dyf + dxf * dxf);
    }

    static boolean pos_equal(Pos a, Pos b) {
        return a.y == b.y && a.x == b.x;
    }

    static boolean contains_pos(Pos[] lst, Pos p) {
        int i_1 = 0;
        while (i_1 < lst.length) {
            if (((Boolean)(pos_equal(lst[i_1], p)))) {
                return true;
            }
            i_1 = i_1 + 1;
        }
        return false;
    }

    static int open_index_of_pos(Node[] open, Pos p) {
        int i_2 = 0;
        while (i_2 < open.length) {
            if (((Boolean)(pos_equal(open[i_2].pos, p)))) {
                return i_2;
            }
            i_2 = i_2 + 1;
        }
        return 0 - 1;
    }

    static Node[] remove_node_at(Node[] nodes, int idx) {
        Node[] res = ((Node[])(new Node[]{}));
        int i_3 = 0;
        while (i_3 < nodes.length) {
            if (i_3 != idx) {
                res = ((Node[])(java.util.stream.Stream.concat(java.util.Arrays.stream(res), java.util.stream.Stream.of(nodes[i_3])).toArray(Node[]::new)));
            }
            i_3 = i_3 + 1;
        }
        return res;
    }

    static Pos[] append_pos_list(Pos[] path, Pos p) {
        Pos[] res_1 = ((Pos[])(new Pos[]{}));
        int i_4 = 0;
        while (i_4 < path.length) {
            res_1 = ((Pos[])(java.util.stream.Stream.concat(java.util.Arrays.stream(res_1), java.util.stream.Stream.of(path[i_4])).toArray(Pos[]::new)));
            i_4 = i_4 + 1;
        }
        res_1 = ((Pos[])(java.util.stream.Stream.concat(java.util.Arrays.stream(res_1), java.util.stream.Stream.of(p)).toArray(Pos[]::new)));
        return res_1;
    }

    static Pos[] reverse_pos_list(Pos[] lst) {
        Pos[] res_2 = ((Pos[])(new Pos[]{}));
        int i_5 = lst.length - 1;
        while (i_5 >= 0) {
            res_2 = ((Pos[])(java.util.stream.Stream.concat(java.util.Arrays.stream(res_2), java.util.stream.Stream.of(lst[i_5])).toArray(Pos[]::new)));
            i_5 = i_5 - 1;
        }
        return res_2;
    }

    static Pos[] concat_pos_lists(Pos[] a, Pos[] b) {
        Pos[] res_3 = ((Pos[])(new Pos[]{}));
        int i_6 = 0;
        while (i_6 < a.length) {
            res_3 = ((Pos[])(java.util.stream.Stream.concat(java.util.Arrays.stream(res_3), java.util.stream.Stream.of(a[i_6])).toArray(Pos[]::new)));
            i_6 = i_6 + 1;
        }
        int j = 0;
        while (j < b.length) {
            res_3 = ((Pos[])(java.util.stream.Stream.concat(java.util.Arrays.stream(res_3), java.util.stream.Stream.of(b[j])).toArray(Pos[]::new)));
            j = j + 1;
        }
        return res_3;
    }

    static Pos[] get_successors(Pos p) {
        Pos[] res_4 = ((Pos[])(new Pos[]{}));
        int i_7 = 0;
        while (i_7 < delta.length) {
            int nx = p.x + delta[i_7][1];
            int ny = p.y + delta[i_7][0];
            if (nx >= 0 && ny >= 0 && nx < grid[0].length && ny < grid.length) {
                if (grid[ny][nx] == 0) {
                    res_4 = ((Pos[])(java.util.stream.Stream.concat(java.util.Arrays.stream(res_4), java.util.stream.Stream.of(new Pos(ny, nx))).toArray(Pos[]::new)));
                }
            }
            i_7 = i_7 + 1;
        }
        return res_4;
    }

    static int find_lowest_f(Node[] open) {
        int best = 0;
        int i_8 = 1;
        while (i_8 < open.length) {
            if (open[i_8].f_cost < open[best].f_cost) {
                best = i_8;
            }
            i_8 = i_8 + 1;
        }
        return best;
    }

    static Pos[] astar(Pos start, Pos goal) {
        double h0 = heuristic(start, goal);
        Node[] open = ((Node[])(new Node[]{new Node(start, 0, h0, h0, new Pos[]{start})}));
        Pos[] closed = ((Pos[])(new Pos[]{}));
        while (open.length > 0) {
            int idx = find_lowest_f(((Node[])(open)));
            Node current = open[idx];
            open = ((Node[])(remove_node_at(((Node[])(open)), idx)));
            if (((Boolean)(pos_equal(current.pos, goal)))) {
                return current.path;
            }
            closed = ((Pos[])(java.util.stream.Stream.concat(java.util.Arrays.stream(closed), java.util.stream.Stream.of(current.pos)).toArray(Pos[]::new)));
            Pos[] succ = ((Pos[])(get_successors(current.pos)));
            int i_9 = 0;
            while (i_9 < succ.length) {
                Pos pos = succ[i_9];
                if (((Boolean)(contains_pos(((Pos[])(closed)), pos)))) {
                    i_9 = i_9 + 1;
                    continue;
                }
                int tentative_g = current.g_cost + 1;
                int idx_open = open_index_of_pos(((Node[])(open)), pos);
                if (idx_open == 0 - 1 || tentative_g < open[idx_open].g_cost) {
                    Pos[] new_path = ((Pos[])(append_pos_list(((Pos[])(current.path)), pos)));
                    double h = heuristic(pos, goal);
                    double f = (((Number)(tentative_g)).doubleValue()) + h;
                    if (idx_open != 0 - 1) {
                        open = ((Node[])(remove_node_at(((Node[])(open)), idx_open)));
                    }
                    open = ((Node[])(java.util.stream.Stream.concat(java.util.Arrays.stream(open), java.util.stream.Stream.of(new Node(pos, tentative_g, h, f, new_path))).toArray(Node[]::new)));
                }
                i_9 = i_9 + 1;
            }
        }
        return new Pos[]{start};
    }

    static Pos[] combine_paths(Node fwd, Node bwd) {
        Pos[] bwd_copy = ((Pos[])(new Pos[]{}));
        int i_10 = 0;
        while (i_10 < bwd.path.length - 1) {
            bwd_copy = ((Pos[])(java.util.stream.Stream.concat(java.util.Arrays.stream(bwd_copy), java.util.stream.Stream.of(bwd.path[i_10])).toArray(Pos[]::new)));
            i_10 = i_10 + 1;
        }
        bwd_copy = ((Pos[])(reverse_pos_list(((Pos[])(bwd_copy)))));
        return concat_pos_lists(((Pos[])(fwd.path)), ((Pos[])(bwd_copy)));
    }

    static Pos[] bidirectional_astar(Pos start, Pos goal) {
        double hf = heuristic(start, goal);
        double hb = heuristic(goal, start);
        Node[] open_f = ((Node[])(new Node[]{new Node(start, 0, hf, hf, new Pos[]{start})}));
        Node[] open_b = ((Node[])(new Node[]{new Node(goal, 0, hb, hb, new Pos[]{goal})}));
        Pos[] closed_f = ((Pos[])(new Pos[]{}));
        Pos[] closed_b = ((Pos[])(new Pos[]{}));
        while (open_f.length > 0 && open_b.length > 0) {
            int idx_f = find_lowest_f(((Node[])(open_f)));
            Node current_f = open_f[idx_f];
            open_f = ((Node[])(remove_node_at(((Node[])(open_f)), idx_f)));
            int idx_b = find_lowest_f(((Node[])(open_b)));
            Node current_b = open_b[idx_b];
            open_b = ((Node[])(remove_node_at(((Node[])(open_b)), idx_b)));
            if (((Boolean)(pos_equal(current_f.pos, current_b.pos)))) {
                return combine_paths(current_f, current_b);
            }
            closed_f = ((Pos[])(java.util.stream.Stream.concat(java.util.Arrays.stream(closed_f), java.util.stream.Stream.of(current_f.pos)).toArray(Pos[]::new)));
            closed_b = ((Pos[])(java.util.stream.Stream.concat(java.util.Arrays.stream(closed_b), java.util.stream.Stream.of(current_b.pos)).toArray(Pos[]::new)));
            Pos[] succ_f = ((Pos[])(get_successors(current_f.pos)));
            int i_11 = 0;
            while (i_11 < succ_f.length) {
                Pos pos_1 = succ_f[i_11];
                if (((Boolean)(contains_pos(((Pos[])(closed_f)), pos_1)))) {
                    i_11 = i_11 + 1;
                    continue;
                }
                int tentative_g_1 = current_f.g_cost + 1;
                double h_1 = heuristic(pos_1, current_b.pos);
                double f_1 = (((Number)(tentative_g_1)).doubleValue()) + h_1;
                int idx_open_1 = open_index_of_pos(((Node[])(open_f)), pos_1);
                if (idx_open_1 == 0 - 1 || tentative_g_1 < open_f[idx_open_1].g_cost) {
                    Pos[] new_path_1 = ((Pos[])(append_pos_list(((Pos[])(current_f.path)), pos_1)));
                    if (idx_open_1 != 0 - 1) {
                        open_f = ((Node[])(remove_node_at(((Node[])(open_f)), idx_open_1)));
                    }
                    open_f = ((Node[])(java.util.stream.Stream.concat(java.util.Arrays.stream(open_f), java.util.stream.Stream.of(new Node(pos_1, tentative_g_1, h_1, f_1, new_path_1))).toArray(Node[]::new)));
                }
                i_11 = i_11 + 1;
            }
            Pos[] succ_b = ((Pos[])(get_successors(current_b.pos)));
            i_11 = 0;
            while (i_11 < succ_b.length) {
                Pos pos_2 = succ_b[i_11];
                if (((Boolean)(contains_pos(((Pos[])(closed_b)), pos_2)))) {
                    i_11 = i_11 + 1;
                    continue;
                }
                int tentative_g_2 = current_b.g_cost + 1;
                double h_2 = heuristic(pos_2, current_f.pos);
                double f_2 = (((Number)(tentative_g_2)).doubleValue()) + h_2;
                int idx_open_2 = open_index_of_pos(((Node[])(open_b)), pos_2);
                if (idx_open_2 == 0 - 1 || tentative_g_2 < open_b[idx_open_2].g_cost) {
                    Pos[] new_path_2 = ((Pos[])(append_pos_list(((Pos[])(current_b.path)), pos_2)));
                    if (idx_open_2 != 0 - 1) {
                        open_b = ((Node[])(remove_node_at(((Node[])(open_b)), idx_open_2)));
                    }
                    open_b = ((Node[])(java.util.stream.Stream.concat(java.util.Arrays.stream(open_b), java.util.stream.Stream.of(new Node(pos_2, tentative_g_2, h_2, f_2, new_path_2))).toArray(Node[]::new)));
                }
                i_11 = i_11 + 1;
            }
        }
        return new Pos[]{start};
    }

    static String path_to_string(Pos[] path) {
        if (path.length == 0) {
            return "[]";
        }
        String s = "[(" + _p(path[0].y) + ", " + _p(path[0].x) + ")";
        int i_12 = 1;
        while (i_12 < path.length) {
            s = s + ", (" + _p(path[i_12].y) + ", " + _p(path[i_12].x) + ")";
            i_12 = i_12 + 1;
        }
        s = s + "]";
        return s;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            HEURISTIC = 0;
            grid = ((int[][])(new int[][]{new int[]{0, 0, 0, 0, 0, 0, 0}, new int[]{0, 1, 0, 0, 0, 0, 0}, new int[]{0, 0, 0, 0, 0, 0, 0}, new int[]{0, 0, 1, 0, 0, 0, 0}, new int[]{1, 0, 1, 0, 0, 0, 0}, new int[]{0, 0, 0, 0, 0, 0, 0}, new int[]{0, 0, 0, 0, 1, 0, 0}}));
            delta = ((int[][])(new int[][]{new int[]{-1, 0}, new int[]{0, -1}, new int[]{1, 0}, new int[]{0, 1}}));
            start = new Pos(0, 0);
            goal = new Pos(grid.length - 1, grid[0].length - 1);
            path1 = ((Pos[])(astar(start, goal)));
            System.out.println(path_to_string(((Pos[])(path1))));
            path2 = ((Pos[])(bidirectional_astar(start, goal)));
            System.out.println(path_to_string(((Pos[])(path2))));
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
