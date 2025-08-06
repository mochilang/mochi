public class Main {
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
        int pos_x;
        int pos_y;
        int goal_x;
        int goal_y;
        int g_cost;
        int f_cost;
        Pos[] path;
        Node(int pos_x, int pos_y, int goal_x, int goal_y, int g_cost, int f_cost, Pos[] path) {
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
    static int[][][] TEST_GRIDS;

    static int abs(int x) {
        if (x < 0) {
            return 0 - x;
        }
        return x;
    }

    static int manhattan(int x1, int y1, int x2, int y2) {
        return Math.abs(x1 - x2) + Math.abs(y1 - y2);
    }

    static Pos[] clone_path(Pos[] p) {
        Pos[] res = ((Pos[])(new Pos[]{}));
        int i = 0;
        while (i < p.length) {
            res = ((Pos[])(java.util.stream.Stream.concat(java.util.Arrays.stream(res), java.util.stream.Stream.of(p[i])).toArray(Pos[]::new)));
            i = i + 1;
        }
        return res;
    }

    static Node make_node(int pos_x, int pos_y, int goal_x, int goal_y, int g_cost, Pos[] path) {
        int f = manhattan(pos_x, pos_y, goal_x, goal_y);
        return new Node(pos_x, pos_y, goal_x, goal_y, g_cost, f, path);
    }

    static boolean node_equal(Node a, Node b) {
        return a.pos_x == b.pos_x && a.pos_y == b.pos_y;
    }

    static boolean contains(Node[] nodes, Node node) {
        int i_1 = 0;
        while (i_1 < nodes.length) {
            if (((Boolean)(node_equal(nodes[i_1], node)))) {
                return true;
            }
            i_1 = i_1 + 1;
        }
        return false;
    }

    static Node[] sort_nodes(Node[] nodes) {
        Node[] arr = ((Node[])(nodes));
        int i_2 = 1;
        while (i_2 < arr.length) {
            Node key_node = arr[i_2];
            int j = i_2 - 1;
            while (j >= 0) {
                Node temp = arr[j];
                if (temp.f_cost > key_node.f_cost) {
arr[j + 1] = temp;
                    j = j - 1;
                } else {
                    break;
                }
            }
arr[j + 1] = key_node;
            i_2 = i_2 + 1;
        }
        return arr;
    }

    static Node[] get_successors(int[][] grid, Node parent, Pos target) {
        Node[] res_1 = ((Node[])(new Node[]{}));
        int i_3 = 0;
        while (i_3 < delta.length) {
            Pos d = delta[i_3];
            int pos_x = parent.pos_x + d.x;
            int pos_y = parent.pos_y + d.y;
            if (pos_x >= 0 && pos_x < grid[0].length && pos_y >= 0 && pos_y < grid.length && grid[pos_y][pos_x] == 0) {
                Pos[] new_path = ((Pos[])(clone_path(((Pos[])(parent.path)))));
                new_path = ((Pos[])(java.util.stream.Stream.concat(java.util.Arrays.stream(new_path), java.util.stream.Stream.of(new Pos(pos_y, pos_x))).toArray(Pos[]::new)));
                res_1 = ((Node[])(java.util.stream.Stream.concat(java.util.Arrays.stream(res_1), java.util.stream.Stream.of(make_node(pos_x, pos_y, target.x, target.y, parent.g_cost + 1, ((Pos[])(new_path))))).toArray(Node[]::new)));
            }
            i_3 = i_3 + 1;
        }
        return res_1;
    }

    static Pos[] greedy_best_first(int[][] grid, Pos init, Pos goal) {
        Pos[] start_path = ((Pos[])(new Pos[]{init}));
        Node start = make_node(init.x, init.y, goal.x, goal.y, 0, ((Pos[])(start_path)));
        Node[] open_nodes = ((Node[])(new Node[]{start}));
        Node[] closed_nodes = ((Node[])(new Node[]{}));
        while (open_nodes.length > 0) {
            open_nodes = ((Node[])(sort_nodes(((Node[])(open_nodes)))));
            Node current = open_nodes[0];
            Node[] new_open = ((Node[])(new Node[]{}));
            int idx = 1;
            while (idx < open_nodes.length) {
                new_open = ((Node[])(java.util.stream.Stream.concat(java.util.Arrays.stream(new_open), java.util.stream.Stream.of(open_nodes[idx])).toArray(Node[]::new)));
                idx = idx + 1;
            }
            open_nodes = ((Node[])(new_open));
            if (current.pos_x == goal.x && current.pos_y == goal.y) {
                return current.path;
            }
            closed_nodes = ((Node[])(java.util.stream.Stream.concat(java.util.Arrays.stream(closed_nodes), java.util.stream.Stream.of(current)).toArray(Node[]::new)));
            Node[] successors = ((Node[])(get_successors(((int[][])(grid)), current, goal)));
            int i_4 = 0;
            while (i_4 < successors.length) {
                Node child = successors[i_4];
                if ((!(Boolean)contains(((Node[])(closed_nodes)), child)) && (!(Boolean)contains(((Node[])(open_nodes)), child))) {
                    open_nodes = ((Node[])(java.util.stream.Stream.concat(java.util.Arrays.stream(open_nodes), java.util.stream.Stream.of(child)).toArray(Node[]::new)));
                }
                i_4 = i_4 + 1;
            }
        }
        Pos[] r = ((Pos[])(new Pos[]{init}));
        return r;
    }

    static void print_grid(int[][] grid) {
        int i_5 = 0;
        while (i_5 < grid.length) {
            System.out.println(_p(_geto(grid, i_5)));
            i_5 = i_5 + 1;
        }
    }

    static void main() {
        int idx_1 = 0;
        while (idx_1 < TEST_GRIDS.length) {
            System.out.println("==grid-" + _p(idx_1 + 1) + "==");
            int[][] grid = ((int[][])(TEST_GRIDS[idx_1]));
            Pos init = new Pos(0, 0);
            Pos goal = new Pos(grid.length - 1, grid[0].length - 1);
            print_grid(((int[][])(grid)));
            System.out.println("------");
            Pos[] path = ((Pos[])(greedy_best_first(((int[][])(grid)), init, goal)));
            int j_1 = 0;
            while (j_1 < path.length) {
                Pos p = path[j_1];
grid[p.y][p.x] = 2;
                j_1 = j_1 + 1;
            }
            print_grid(((int[][])(grid)));
            idx_1 = idx_1 + 1;
        }
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            delta = ((Pos[])(new Pos[]{new Pos(-1, 0), new Pos(0, -1), new Pos(1, 0), new Pos(0, 1)}));
            TEST_GRIDS = ((int[][][])(new int[][][]{new int[][]{new int[]{0, 0, 0, 0, 0, 0, 0}, new int[]{0, 1, 0, 0, 0, 0, 0}, new int[]{0, 0, 0, 0, 0, 0, 0}, new int[]{0, 0, 1, 0, 0, 0, 0}, new int[]{1, 0, 1, 0, 0, 0, 0}, new int[]{0, 0, 0, 0, 0, 0, 0}, new int[]{0, 0, 0, 0, 1, 0, 0}}, new int[][]{new int[]{0, 0, 0, 1, 1, 0, 0}, new int[]{0, 0, 0, 0, 1, 0, 1}, new int[]{0, 0, 0, 1, 1, 0, 0}, new int[]{0, 1, 0, 0, 1, 0, 0}, new int[]{1, 0, 0, 1, 1, 0, 1}, new int[]{0, 0, 0, 0, 0, 0, 0}}, new int[][]{new int[]{0, 0, 1, 0, 0}, new int[]{0, 1, 0, 0, 0}, new int[]{0, 0, 1, 0, 1}, new int[]{1, 0, 0, 1, 1}, new int[]{0, 0, 0, 0, 0}}}));
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
        return String.valueOf(v);
    }

    static Object _geto(Object[] a, int i) {
        return (i >= 0 && i < a.length) ? a[i] : null;
    }
}
