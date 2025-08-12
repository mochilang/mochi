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

    static long world_x;
    static long world_y;
    static Point start;
    static Point goal;
    static Point[] path_2;
    static long[][] world_1;

    static Point[] get_neighbours(Point p, long x_limit, long y_limit) {
        Point[] deltas = ((Point[])(new Point[]{new Point((0 - 1), (0 - 1)), new Point((0 - 1), 0), new Point((0 - 1), 1), new Point(0, (0 - 1)), new Point(0, 1), new Point(1, (0 - 1)), new Point(1, 0), new Point(1, 1)}));
        Point[] neighbours_1 = ((Point[])(new Point[]{}));
        for (Point d : deltas) {
            long nx_1 = p.x + d.x;
            long ny_1 = p.y + d.y;
            if (0 <= nx_1 && nx_1 < x_limit && 0 <= ny_1 && ny_1 < y_limit) {
                neighbours_1 = ((Point[])(java.util.stream.Stream.concat(java.util.Arrays.stream(neighbours_1), java.util.stream.Stream.of(new Point(nx_1, ny_1))).toArray(Point[]::new)));
            }
        }
        return neighbours_1;
    }

    static boolean contains(Node[] nodes, Point p) {
        for (Node n : nodes) {
            if (n.pos.x == p.x && n.pos.y == p.y) {
                return true;
            }
        }
        return false;
    }

    static Node get_node(Node[] nodes, Point p) {
        for (Node n : nodes) {
            if (n.pos.x == p.x && n.pos.y == p.y) {
                return n;
            }
        }
        return new Node(p, new Point((0 - 1), (0 - 1)), 0, 0, 0);
    }

    static Point[] astar(long x_limit, long y_limit, Point start, Point goal) {
        Node[] open = ((Node[])(new Node[]{}));
        Node[] closed_1 = ((Node[])(new Node[]{}));
        open = ((Node[])(java.util.stream.Stream.concat(java.util.Arrays.stream(open), java.util.stream.Stream.of(new Node(start, new Point((0 - 1), (0 - 1)), 0, 0, 0))).toArray(Node[]::new)));
        Node current_1 = open[(int)((long)(0))];
        while (open.length > 0) {
            long min_index_1 = 0L;
            long i_1 = 1L;
            while (i_1 < open.length) {
                if (open[(int)((long)(i_1))].f < open[(int)((long)(min_index_1))].f) {
                    min_index_1 = i_1;
                }
                i_1 = i_1 + 1;
            }
            current_1 = open[(int)((long)(min_index_1))];
            Node[] new_open_1 = ((Node[])(new Node[]{}));
            long j_1 = 0L;
            while (j_1 < open.length) {
                if (j_1 != min_index_1) {
                    new_open_1 = ((Node[])(java.util.stream.Stream.concat(java.util.Arrays.stream(new_open_1), java.util.stream.Stream.of(open[(int)((long)(j_1))])).toArray(Node[]::new)));
                }
                j_1 = j_1 + 1;
            }
            open = ((Node[])(new_open_1));
            closed_1 = ((Node[])(java.util.stream.Stream.concat(java.util.Arrays.stream(closed_1), java.util.stream.Stream.of(current_1)).toArray(Node[]::new)));
            if (current_1.pos.x == goal.x && current_1.pos.y == goal.y) {
                break;
            }
            Point[] neighbours_3 = ((Point[])(get_neighbours(current_1.pos, x_limit, y_limit)));
            for (Point np : neighbours_3) {
                if (((Boolean)(contains(((Node[])(closed_1)), np)))) {
                    continue;
                }
                long g_1 = current_1.g + 1;
                long dx_1 = goal.x - np.x;
                long dy_1 = goal.y - np.y;
                long h_1 = dx_1 * dx_1 + dy_1 * dy_1;
                long f_1 = g_1 + h_1;
                boolean skip_1 = false;
                for (Node node : open) {
                    if (node.pos.x == np.x && node.pos.y == np.y && node.f < f_1) {
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
        while (!(current_1.parent.x == (0 - 1) && current_1.parent.y == (0 - 1))) {
            current_1 = get_node(((Node[])(closed_1)), current_1.parent);
            path_1 = ((Point[])(java.util.stream.Stream.concat(java.util.Arrays.stream(path_1), java.util.stream.Stream.of(current_1.pos)).toArray(Point[]::new)));
        }
        Point[] rev_1 = ((Point[])(new Point[]{}));
        long k_1 = path_1.length - 1;
        while (k_1 >= 0) {
            rev_1 = ((Point[])(java.util.stream.Stream.concat(java.util.Arrays.stream(rev_1), java.util.stream.Stream.of(path_1[(int)((long)(k_1))])).toArray(Point[]::new)));
            k_1 = k_1 - 1;
        }
        return rev_1;
    }

    static long[][] create_world(long x_limit, long y_limit) {
        long[][] world = ((long[][])(new long[][]{}));
        long i_3 = 0L;
        while (i_3 < x_limit) {
            long[] row_1 = ((long[])(new long[]{}));
            long j_3 = 0L;
            while (j_3 < y_limit) {
                row_1 = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(row_1), java.util.stream.LongStream.of(0L)).toArray()));
                j_3 = j_3 + 1;
            }
            world = ((long[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(world), java.util.stream.Stream.of(row_1)).toArray(long[][]::new)));
            i_3 = i_3 + 1;
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
        world_x = 5;
        world_y = 5;
        start = new Point(0, 0);
        goal = new Point(4, 4);
        path_2 = ((Point[])(astar(world_x, world_y, start, goal)));
        System.out.println("path from (" + _p(start.x) + ", " + _p(start.y) + ") to (" + _p(goal.x) + ", " + _p(goal.y) + ")");
        world_1 = ((long[][])(create_world(world_x, world_y)));
        mark_path(((long[][])(world_1)), ((Point[])(path_2)));
        print_world(((long[][])(world_1)));
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
            if (d == Math.rint(d)) return String.valueOf((long) d);
            return String.valueOf(d);
        }
        return String.valueOf(v);
    }
}
