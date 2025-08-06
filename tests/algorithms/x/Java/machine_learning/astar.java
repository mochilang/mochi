public class Main {
    static class Point {
        int x;
        int y;
        Point(int x, int y) {
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
        int g;
        int h;
        int f;
        Node(Point pos, Point parent, int g, int h, int f) {
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

    static int world_x;
    static int world_y;
    static Point start;
    static Point goal;
    static Point[] path_1;
    static int[][] world_1;

    static Point[] get_neighbours(Point p, int x_limit, int y_limit) {
        Point[] deltas = ((Point[])(new Point[]{new Point((0 - 1), (0 - 1)), new Point((0 - 1), 0), new Point((0 - 1), 1), new Point(0, (0 - 1)), new Point(0, 1), new Point(1, (0 - 1)), new Point(1, 0), new Point(1, 1)}));
        Point[] neighbours = ((Point[])(new Point[]{}));
        for (Point d : deltas) {
            int nx = p.x + d.x;
            int ny = p.y + d.y;
            if (0 <= nx && nx < x_limit && 0 <= ny && ny < y_limit) {
                neighbours = ((Point[])(java.util.stream.Stream.concat(java.util.Arrays.stream(neighbours), java.util.stream.Stream.of(new Point(nx, ny))).toArray(Point[]::new)));
            }
        }
        return neighbours;
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

    static Point[] astar(int x_limit, int y_limit, Point start, Point goal) {
        Node[] open = ((Node[])(new Node[]{}));
        Node[] closed = ((Node[])(new Node[]{}));
        open = ((Node[])(java.util.stream.Stream.concat(java.util.Arrays.stream(open), java.util.stream.Stream.of(new Node(start, new Point((0 - 1), (0 - 1)), 0, 0, 0))).toArray(Node[]::new)));
        Node current = open[0];
        while (open.length > 0) {
            int min_index = 0;
            int i = 1;
            while (i < open.length) {
                if (open[i].f < open[min_index].f) {
                    min_index = i;
                }
                i = i + 1;
            }
            current = open[min_index];
            Node[] new_open = ((Node[])(new Node[]{}));
            int j = 0;
            while (j < open.length) {
                if (j != min_index) {
                    new_open = ((Node[])(java.util.stream.Stream.concat(java.util.Arrays.stream(new_open), java.util.stream.Stream.of(open[j])).toArray(Node[]::new)));
                }
                j = j + 1;
            }
            open = ((Node[])(new_open));
            closed = ((Node[])(java.util.stream.Stream.concat(java.util.Arrays.stream(closed), java.util.stream.Stream.of(current)).toArray(Node[]::new)));
            if (current.pos.x == goal.x && current.pos.y == goal.y) {
                break;
            }
            Point[] neighbours_1 = ((Point[])(get_neighbours(current.pos, x_limit, y_limit)));
            for (Point np : neighbours_1) {
                if (((Boolean)(contains(((Node[])(closed)), np)))) {
                    continue;
                }
                int g = current.g + 1;
                int dx = goal.x - np.x;
                int dy = goal.y - np.y;
                int h = dx * dx + dy * dy;
                int f = g + h;
                boolean skip = false;
                for (Node node : open) {
                    if (node.pos.x == np.x && node.pos.y == np.y && node.f < f) {
                        skip = true;
                    }
                }
                if (skip) {
                    continue;
                }
                open = ((Node[])(java.util.stream.Stream.concat(java.util.Arrays.stream(open), java.util.stream.Stream.of(new Node(np, current.pos, g, h, f))).toArray(Node[]::new)));
            }
        }
        Point[] path = ((Point[])(new Point[]{}));
        path = ((Point[])(java.util.stream.Stream.concat(java.util.Arrays.stream(path), java.util.stream.Stream.of(current.pos)).toArray(Point[]::new)));
        while (!(current.parent.x == (0 - 1) && current.parent.y == (0 - 1))) {
            current = get_node(((Node[])(closed)), current.parent);
            path = ((Point[])(java.util.stream.Stream.concat(java.util.Arrays.stream(path), java.util.stream.Stream.of(current.pos)).toArray(Point[]::new)));
        }
        Point[] rev = ((Point[])(new Point[]{}));
        int k = path.length - 1;
        while (k >= 0) {
            rev = ((Point[])(java.util.stream.Stream.concat(java.util.Arrays.stream(rev), java.util.stream.Stream.of(path[k])).toArray(Point[]::new)));
            k = k - 1;
        }
        return rev;
    }

    static int[][] create_world(int x_limit, int y_limit) {
        int[][] world = ((int[][])(new int[][]{}));
        int i_1 = 0;
        while (i_1 < x_limit) {
            int[] row = ((int[])(new int[]{}));
            int j_1 = 0;
            while (j_1 < y_limit) {
                row = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(row), java.util.stream.IntStream.of(0)).toArray()));
                j_1 = j_1 + 1;
            }
            world = ((int[][])(appendObj(world, row)));
            i_1 = i_1 + 1;
        }
        return world;
    }

    static void mark_path(int[][] world, Point[] path) {
        for (Point p : path) {
world[p.x][p.y] = 1;
        }
    }

    static void print_world(int[][] world) {
        for (int[] row : world) {
            System.out.println(_p(row));
        }
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            world_x = 5;
            world_y = 5;
            start = new Point(0, 0);
            goal = new Point(4, 4);
            path_1 = ((Point[])(astar(world_x, world_y, start, goal)));
            System.out.println("path from (" + _p(start.x) + ", " + _p(start.y) + ") to (" + _p(goal.x) + ", " + _p(goal.y) + ")");
            world_1 = ((int[][])(create_world(world_x, world_y)));
            mark_path(((int[][])(world_1)), ((Point[])(path_1)));
            print_world(((int[][])(world_1)));
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

    static <T> T[] appendObj(T[] arr, T v) {
        T[] out = java.util.Arrays.copyOf(arr, arr.length + 1);
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
        return String.valueOf(v);
    }
}
