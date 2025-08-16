public class Main {
    static class Point {
        long x;
        long y;
        Point(long x, long y) {
            this.x = x;
            this.y = y;
        }
        Point() {}
        @Override public String toString() {
            return String.format("{'x': %s, 'y': %s}", String.valueOf(x), String.valueOf(y));
        }
    }

    static class Node {
        Point pos;
        Point parent;
        long g;
        long h;
        long f;
        Node(Point pos, Point parent, long g, long h, long f) {
            this.pos = pos;
            this.parent = parent;
            this.g = g;
            this.h = h;
            this.f = f;
        }
        Node() {}
        @Override public String toString() {
            return String.format("{'pos': %s, 'parent': %s, 'g': %s, 'h': %s, 'f': %s}", String.valueOf(pos), String.valueOf(parent), String.valueOf(g), String.valueOf(h), String.valueOf(f));
        }
    }

    static long world_x = 5L;
    static long world_y = 5L;
    static Point start;
    static Point goal;
    static Point[] path_2;
    static long[][] world_1;

    static Point[] get_neighbours(Point p, long x_limit, long y_limit) {
        Point[] deltas = ((Point[])(new Point[]{new Point((0L - 1L), (0L - 1L)), new Point((0L - 1L), 0), new Point((0L - 1L), 1), new Point(0, (0L - 1L)), new Point(0, 1), new Point(1, (0L - 1L)), new Point(1, 0), new Point(1, 1)}));
        Point[] neighbours_1 = ((Point[])(new Point[]{}));
        for (Point d : deltas) {
            long nx_1 = (long)((long)(p.x) + (long)(d.x));
            long ny_1 = (long)((long)(p.y) + (long)(d.y));
            if (0L <= (long)(nx_1) && (long)(nx_1) < (long)(x_limit) && 0L <= (long)(ny_1) && (long)(ny_1) < (long)(y_limit)) {
                neighbours_1 = ((Point[])(java.util.stream.Stream.concat(java.util.Arrays.stream(neighbours_1), java.util.stream.Stream.of(new Point(nx_1, ny_1))).toArray(Point[]::new)));
            }
        }
        return neighbours_1;
    }

    static boolean contains(Node[] nodes, Point p) {
        for (Node n : nodes) {
            if ((long)(n.pos.x) == (long)(p.x) && (long)(n.pos.y) == (long)(p.y)) {
                return true;
            }
        }
        return false;
    }

    static Node get_node(Node[] nodes, Point p) {
        for (Node n : nodes) {
            if ((long)(n.pos.x) == (long)(p.x) && (long)(n.pos.y) == (long)(p.y)) {
                return n;
            }
        }
        return new Node(p, new Point((0L - 1L), (0L - 1L)), 0, 0, 0);
    }

    static Point[] astar(long x_limit, long y_limit, Point start, Point goal) {
        Node[] open = ((Node[])(new Node[]{}));
        Node[] closed_1 = ((Node[])(new Node[]{}));
        open = ((Node[])(java.util.stream.Stream.concat(java.util.Arrays.stream(open), java.util.stream.Stream.of(new Node(start, new Point((0L - 1L), (0L - 1L)), 0, 0, 0))).toArray(Node[]::new)));
        Node current_1 = open[(int)(0L)];
        while ((long)(open.length) > 0L) {
            long min_index_1 = 0L;
            long i_1 = 1L;
            while ((long)(i_1) < (long)(open.length)) {
                if ((long)(open[(int)((long)(i_1))].f) < (long)(open[(int)((long)(min_index_1))].f)) {
                    min_index_1 = (long)(i_1);
                }
                i_1 = (long)((long)(i_1) + 1L);
            }
            current_1 = open[(int)((long)(min_index_1))];
            Node[] new_open_1 = ((Node[])(new Node[]{}));
            long j_1 = 0L;
            while ((long)(j_1) < (long)(open.length)) {
                if ((long)(j_1) != (long)(min_index_1)) {
                    new_open_1 = ((Node[])(java.util.stream.Stream.concat(java.util.Arrays.stream(new_open_1), java.util.stream.Stream.of(open[(int)((long)(j_1))])).toArray(Node[]::new)));
                }
                j_1 = (long)((long)(j_1) + 1L);
            }
            open = ((Node[])(new_open_1));
            closed_1 = ((Node[])(java.util.stream.Stream.concat(java.util.Arrays.stream(closed_1), java.util.stream.Stream.of(current_1)).toArray(Node[]::new)));
            if ((long)(current_1.pos.x) == (long)(goal.x) && (long)(current_1.pos.y) == (long)(goal.y)) {
                break;
            }
            Point[] neighbours_3 = ((Point[])(get_neighbours(current_1.pos, (long)(x_limit), (long)(y_limit))));
            for (Point np : neighbours_3) {
                if (contains(((Node[])(closed_1)), np)) {
                    continue;
                }
                long g_1 = (long)((long)(current_1.g) + 1L);
                long dx_1 = (long)((long)(goal.x) - (long)(np.x));
                long dy_1 = (long)((long)(goal.y) - (long)(np.y));
                long h_1 = (long)((long)((long)(dx_1) * (long)(dx_1)) + (long)((long)(dy_1) * (long)(dy_1)));
                long f_1 = (long)((long)(g_1) + (long)(h_1));
                boolean skip_1 = false;
                for (Node node : open) {
                    if ((long)(node.pos.x) == (long)(np.x) && (long)(node.pos.y) == (long)(np.y) && (long)(node.f) < (long)(f_1)) {
                        skip_1 = true;
                    }
                }
                if (skip_1) {
                    continue;
                }
                open = ((Node[])(java.util.stream.Stream.concat(java.util.Arrays.stream(open), java.util.stream.Stream.of(new Node(np, current_1.pos, g_1, h_1, f_1))).toArray(Node[]::new)));
            }
        }
        Point[] path_1 = ((Point[])(new Point[]{}));
        path_1 = ((Point[])(java.util.stream.Stream.concat(java.util.Arrays.stream(path_1), java.util.stream.Stream.of(current_1.pos)).toArray(Point[]::new)));
        while (!((long)(current_1.parent.x) == (long)((0L - 1L)) && (long)(current_1.parent.y) == (long)((0L - 1L)))) {
            current_1 = get_node(((Node[])(closed_1)), current_1.parent);
            path_1 = ((Point[])(java.util.stream.Stream.concat(java.util.Arrays.stream(path_1), java.util.stream.Stream.of(current_1.pos)).toArray(Point[]::new)));
        }
        Point[] rev_1 = ((Point[])(new Point[]{}));
        long k_1 = (long)((long)(path_1.length) - 1L);
        while ((long)(k_1) >= 0L) {
            rev_1 = ((Point[])(java.util.stream.Stream.concat(java.util.Arrays.stream(rev_1), java.util.stream.Stream.of(path_1[(int)((long)(k_1))])).toArray(Point[]::new)));
            k_1 = (long)((long)(k_1) - 1L);
        }
        return rev_1;
    }

    static long[][] create_world(long x_limit, long y_limit) {
        long[][] world = ((long[][])(new long[][]{}));
        long i_3 = 0L;
        while ((long)(i_3) < (long)(x_limit)) {
            long[] row_1 = ((long[])(new long[]{}));
            long j_3 = 0L;
            while ((long)(j_3) < (long)(y_limit)) {
                row_1 = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(row_1), java.util.stream.LongStream.of(0L)).toArray()));
                j_3 = (long)((long)(j_3) + 1L);
            }
            world = ((long[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(world), java.util.stream.Stream.of(new long[][]{row_1})).toArray(long[][]::new)));
            i_3 = (long)((long)(i_3) + 1L);
        }
        return world;
    }

    static void mark_path(long[][] world, Point[] path) {
        for (Point p : path) {
world[(int)((long)(p.x))][(int)((long)(p.y))] = 1L;
        }
    }

    static void print_world(long[][] world) {
        for (long[] row : world) {
            System.out.println(_p(row));
        }
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            start = new Point(0, 0);
            goal = new Point(4, 4);
            path_2 = ((Point[])(astar((long)(world_x), (long)(world_y), start, goal)));
            System.out.println("path from (" + _p(start.x) + ", " + _p(start.y) + ") to (" + _p(goal.x) + ", " + _p(goal.y) + ")");
            world_1 = ((long[][])(create_world((long)(world_x), (long)(world_y))));
            mark_path(((long[][])(world_1)), ((Point[])(path_2)));
            print_world(((long[][])(world_1)));
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
