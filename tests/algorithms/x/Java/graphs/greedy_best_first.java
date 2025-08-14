public class Main {
    static class Pos {
        long y;
        long x;
        Pos(long y, long x) {
            this.y = y;
            this.x = x;
        }
        Pos() {}
        @Override public String toString() {
            return String.format("{'y': %s, 'x': %s}", String.valueOf(y), String.valueOf(x));
        }
    }

    static class Node {
        long pos_x;
        long pos_y;
        long goal_x;
        long goal_y;
        long g_cost;
        long f_cost;
        Pos[] path;
        Node(long pos_x, long pos_y, long goal_x, long goal_y, long g_cost, long f_cost, Pos[] path) {
            this.pos_x = pos_x;
            this.pos_y = pos_y;
            this.goal_x = goal_x;
            this.goal_y = goal_y;
            this.g_cost = g_cost;
            this.f_cost = f_cost;
            this.path = path;
        }
        Node() {}
        @Override public String toString() {
            return String.format("{'pos_x': %s, 'pos_y': %s, 'goal_x': %s, 'goal_y': %s, 'g_cost': %s, 'f_cost': %s, 'path': %s}", String.valueOf(pos_x), String.valueOf(pos_y), String.valueOf(goal_x), String.valueOf(goal_y), String.valueOf(g_cost), String.valueOf(f_cost), String.valueOf(path));
        }
    }

    static Pos[] delta;
    static long[][][] TEST_GRIDS;

    static long abs(long x) {
        if ((long)(x) < 0L) {
            return 0L - (long)(x);
        }
        return x;
    }

    static long manhattan(long x1, long y1, long x2, long y2) {
        return Math.abs((long)(x1) - (long)(x2)) + Math.abs((long)(y1) - (long)(y2));
    }

    static Pos[] clone_path(Pos[] p) {
        Pos[] res = ((Pos[])(new Pos[]{}));
        long i_1 = 0L;
        while ((long)(i_1) < (long)(p.length)) {
            res = ((Pos[])(java.util.stream.Stream.concat(java.util.Arrays.stream(res), java.util.stream.Stream.of(p[(int)((long)(i_1))])).toArray(Pos[]::new)));
            i_1 = (long)((long)(i_1) + 1L);
        }
        return res;
    }

    static Node make_node(long pos_x, long pos_y, long goal_x, long goal_y, long g_cost, Pos[] path) {
        long f = (long)(manhattan((long)(pos_x), (long)(pos_y), (long)(goal_x), (long)(goal_y)));
        return new Node(pos_x, pos_y, goal_x, goal_y, g_cost, f, path);
    }

    static boolean node_equal(Node a, Node b) {
        return (long)(a.pos_x) == (long)(b.pos_x) && (long)(a.pos_y) == (long)(b.pos_y);
    }

    static boolean contains(Node[] nodes, Node node) {
        long i_2 = 0L;
        while ((long)(i_2) < (long)(nodes.length)) {
            if (node_equal(nodes[(int)((long)(i_2))], node)) {
                return true;
            }
            i_2 = (long)((long)(i_2) + 1L);
        }
        return false;
    }

    static Node[] sort_nodes(Node[] nodes) {
        Node[] arr = ((Node[])(nodes));
        long i_4 = 1L;
        while ((long)(i_4) < (long)(arr.length)) {
            Node key_node_1 = arr[(int)((long)(i_4))];
            long j_1 = (long)((long)(i_4) - 1L);
            while ((long)(j_1) >= 0L) {
                Node temp_1 = arr[(int)((long)(j_1))];
                if ((long)(temp_1.f_cost) > (long)(key_node_1.f_cost)) {
arr[(int)((long)((long)(j_1) + 1L))] = temp_1;
                    j_1 = (long)((long)(j_1) - 1L);
                } else {
                    break;
                }
            }
arr[(int)((long)((long)(j_1) + 1L))] = key_node_1;
            i_4 = (long)((long)(i_4) + 1L);
        }
        return arr;
    }

    static Node[] get_successors(long[][] grid, Node parent, Pos target) {
        Node[] res_1 = ((Node[])(new Node[]{}));
        long i_6 = 0L;
        while ((long)(i_6) < (long)(delta.length)) {
            Pos d_1 = delta[(int)((long)(i_6))];
            long pos_x_1 = (long)((long)(parent.pos_x) + (long)(d_1.x));
            long pos_y_1 = (long)((long)(parent.pos_y) + (long)(d_1.y));
            if ((long)(pos_x_1) >= 0L && (long)(pos_x_1) < (long)(grid[(int)((long)(0))].length) && (long)(pos_y_1) >= 0L && (long)(pos_y_1) < (long)(grid.length) && (long)(grid[(int)((long)(pos_y_1))][(int)((long)(pos_x_1))]) == 0L) {
                Pos[] new_path_1 = ((Pos[])(clone_path(((Pos[])(parent.path)))));
                new_path_1 = ((Pos[])(java.util.stream.Stream.concat(java.util.Arrays.stream(new_path_1), java.util.stream.Stream.of(new Pos(pos_y_1, pos_x_1))).toArray(Pos[]::new)));
                res_1 = ((Node[])(java.util.stream.Stream.concat(java.util.Arrays.stream(res_1), java.util.stream.Stream.of(make_node((long)(pos_x_1), (long)(pos_y_1), (long)(target.x), (long)(target.y), (long)((long)(parent.g_cost) + 1L), ((Pos[])(new_path_1))))).toArray(Node[]::new)));
            }
            i_6 = (long)((long)(i_6) + 1L);
        }
        return res_1;
    }

    static Pos[] greedy_best_first(long[][] grid, Pos init, Pos goal) {
        Pos[] start_path = ((Pos[])(new Pos[]{init}));
        Node start_1 = make_node((long)(init.x), (long)(init.y), (long)(goal.x), (long)(goal.y), 0L, ((Pos[])(start_path)));
        Node[] open_nodes_1 = ((Node[])(new Node[]{start_1}));
        Node[] closed_nodes_1 = ((Node[])(new Node[]{}));
        while ((long)(open_nodes_1.length) > 0L) {
            open_nodes_1 = ((Node[])(sort_nodes(((Node[])(open_nodes_1)))));
            Node current_1 = open_nodes_1[(int)((long)(0))];
            Node[] new_open_1 = ((Node[])(new Node[]{}));
            long idx_1 = 1L;
            while ((long)(idx_1) < (long)(open_nodes_1.length)) {
                new_open_1 = ((Node[])(java.util.stream.Stream.concat(java.util.Arrays.stream(new_open_1), java.util.stream.Stream.of(open_nodes_1[(int)((long)(idx_1))])).toArray(Node[]::new)));
                idx_1 = (long)((long)(idx_1) + 1L);
            }
            open_nodes_1 = ((Node[])(new_open_1));
            if ((long)(current_1.pos_x) == (long)(goal.x) && (long)(current_1.pos_y) == (long)(goal.y)) {
                return current_1.path;
            }
            closed_nodes_1 = ((Node[])(java.util.stream.Stream.concat(java.util.Arrays.stream(closed_nodes_1), java.util.stream.Stream.of(current_1)).toArray(Node[]::new)));
            Node[] successors_1 = ((Node[])(get_successors(((long[][])(grid)), current_1, goal)));
            long i_8 = 0L;
            while ((long)(i_8) < (long)(successors_1.length)) {
                Node child_1 = successors_1[(int)((long)(i_8))];
                if ((!(Boolean)contains(((Node[])(closed_nodes_1)), child_1)) && (!(Boolean)contains(((Node[])(open_nodes_1)), child_1))) {
                    open_nodes_1 = ((Node[])(java.util.stream.Stream.concat(java.util.Arrays.stream(open_nodes_1), java.util.stream.Stream.of(child_1)).toArray(Node[]::new)));
                }
                i_8 = (long)((long)(i_8) + 1L);
            }
        }
        Pos[] r_1 = ((Pos[])(new Pos[]{init}));
        return r_1;
    }

    static void print_grid(long[][] grid) {
        long i_9 = 0L;
        while ((long)(i_9) < (long)(grid.length)) {
            System.out.println(_p(_getd(grid, ((Number)(i_9)).intValue())));
            i_9 = (long)((long)(i_9) + 1L);
        }
    }

    static void main() {
        long idx_2 = 0L;
        while ((long)(idx_2) < (long)(TEST_GRIDS.length)) {
            System.out.println("==grid-" + _p((long)(idx_2) + 1L) + "==");
            long[][] grid_1 = ((long[][])(TEST_GRIDS[(int)((long)(idx_2))]));
            Pos init_1 = new Pos(0, 0);
            Pos goal_1 = new Pos((long)(grid_1.length) - 1L, (long)(grid_1[(int)((long)(0))].length) - 1L);
            print_grid(((long[][])(grid_1)));
            System.out.println("------");
            Pos[] path_1 = ((Pos[])(greedy_best_first(((long[][])(grid_1)), init_1, goal_1)));
            long j_3 = 0L;
            while ((long)(j_3) < (long)(path_1.length)) {
                Pos p_1 = path_1[(int)((long)(j_3))];
grid_1[(int)((long)(p_1.y))][(int)((long)(p_1.x))] = 2L;
                j_3 = (long)((long)(j_3) + 1L);
            }
            print_grid(((long[][])(grid_1)));
            idx_2 = (long)((long)(idx_2) + 1L);
        }
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            delta = ((Pos[])(new Pos[]{new Pos(-1, 0), new Pos(0, -1), new Pos(1, 0), new Pos(0, 1)}));
            TEST_GRIDS = ((long[][][])(new long[][][]{new long[][]{new long[]{0, 0, 0, 0, 0, 0, 0}, new long[]{0, 1, 0, 0, 0, 0, 0}, new long[]{0, 0, 0, 0, 0, 0, 0}, new long[]{0, 0, 1, 0, 0, 0, 0}, new long[]{1, 0, 1, 0, 0, 0, 0}, new long[]{0, 0, 0, 0, 0, 0, 0}, new long[]{0, 0, 0, 0, 1, 0, 0}}, new long[][]{new long[]{0, 0, 0, 1, 1, 0, 0}, new long[]{0, 0, 0, 0, 1, 0, 1}, new long[]{0, 0, 0, 1, 1, 0, 0}, new long[]{0, 1, 0, 0, 1, 0, 0}, new long[]{1, 0, 0, 1, 1, 0, 1}, new long[]{0, 0, 0, 0, 0, 0, 0}}, new long[][]{new long[]{0, 0, 1, 0, 0}, new long[]{0, 1, 0, 0, 0}, new long[]{0, 0, 1, 0, 1}, new long[]{1, 0, 0, 1, 1}, new long[]{0, 0, 0, 0, 0}}}));
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

    static Double _getd(double[] a, int i) {
        return (i >= 0 && i < a.length) ? a[i] : null;
    }
}
