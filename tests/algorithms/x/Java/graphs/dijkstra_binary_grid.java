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

    static class Result {
        double distance;
        Point[] path;
        Result(double distance, Point[] path) {
            this.distance = distance;
            this.path = path;
        }
        Result() {}
        @Override public String toString() {
            return String.format("{'distance': %s, 'path': %s}", String.valueOf(distance), String.valueOf(path));
        }
    }

    static int[][] grid1 = new int[0][];
    static int[][] grid2 = new int[0][];

    static String key(Point p) {
        return _p(p.x) + "," + _p(p.y);
    }

    static String path_to_string(Point[] path) {
        String s = "[";
        int i = 0;
        while (i < path.length) {
            Point pt = path[i];
            s = s + "(" + _p(pt.x) + ", " + _p(pt.y) + ")";
            if (i < path.length - 1) {
                s = s + ", ";
            }
            i = i + 1;
        }
        s = s + "]";
        return s;
    }

    static Result dijkstra(int[][] grid, Point source, Point destination, boolean allow_diagonal) {
        int rows = grid.length;
        int cols = grid[0].length;
        int[] dx = ((int[])(new int[]{-1, 1, 0, 0}));
        int[] dy = ((int[])(new int[]{0, 0, -1, 1}));
        if (((Boolean)(allow_diagonal))) {
            dx = ((int[])(concat(dx, new int[]{-1, -1, 1, 1})));
            dy = ((int[])(concat(dy, new int[]{-1, 1, -1, 1})));
        }
        double INF = 1000000000000.0;
        Point[] queue = ((Point[])(new Point[]{source}));
        int front = 0;
        java.util.Map<String,Double> dist_map = ((java.util.Map<String,Double>)(new java.util.LinkedHashMap<String, Double>(java.util.Map.ofEntries(java.util.Map.entry(key(source), 0.0)))));
        java.util.Map<String,Point> prev = ((java.util.Map<String,Point>)(new java.util.LinkedHashMap<String, Point>()));
        while (front < queue.length) {
            Point current = queue[front];
            front = front + 1;
            String cur_key = String.valueOf(key(current));
            if (current.x == destination.x && current.y == destination.y) {
                break;
            }
            int i_1 = 0;
            while (i_1 < dx.length) {
                int nx = current.x + dx[i_1];
                int ny = current.y + dy[i_1];
                if (nx >= 0 && nx < rows && ny >= 0 && ny < cols) {
                    if (grid[nx][ny] == 1) {
                        String n_key = _p(nx) + "," + _p(ny);
                        if (!(Boolean)(dist_map.containsKey(n_key))) {
dist_map.put(n_key, (double)(((double)(dist_map).getOrDefault(cur_key, 0.0))) + 1.0);
prev.put(n_key, current);
                            queue = ((Point[])(java.util.stream.Stream.concat(java.util.Arrays.stream(queue), java.util.stream.Stream.of(new Point(nx, ny))).toArray(Point[]::new)));
                        }
                    }
                }
                i_1 = i_1 + 1;
            }
        }
        String dest_key = String.valueOf(key(destination));
        if (((Boolean)(dist_map.containsKey(dest_key)))) {
            Point[] path_rev = ((Point[])(new Point[]{destination}));
            String step_key = dest_key;
            Point step_pt = destination;
            while (!(step_key.equals(key(source)))) {
                step_pt = (Point)(((Point)(prev).get(step_key)));
                step_key = String.valueOf(key(step_pt));
                path_rev = ((Point[])(java.util.stream.Stream.concat(java.util.Arrays.stream(path_rev), java.util.stream.Stream.of(step_pt)).toArray(Point[]::new)));
            }
            Point[] path = ((Point[])(new Point[]{}));
            int k = path_rev.length - 1;
            while (k >= 0) {
                path = ((Point[])(java.util.stream.Stream.concat(java.util.Arrays.stream(path), java.util.stream.Stream.of(path_rev[k])).toArray(Point[]::new)));
                k = k - 1;
            }
            return new Result(((double)(dist_map).getOrDefault(dest_key, 0.0)), path);
        }
        return new Result(INF, new Point[]{});
    }

    static void print_result(Result res) {
        System.out.println(_p(res.distance) + ", " + String.valueOf(path_to_string(((Point[])(res.path)))));
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            grid1 = ((int[][])(new int[][]{new int[]{1, 1, 1}, new int[]{0, 1, 0}, new int[]{0, 1, 1}}));
            print_result(dijkstra(((int[][])(grid1)), new Point(0, 0), new Point(2, 2), false));
            print_result(dijkstra(((int[][])(grid1)), new Point(0, 0), new Point(2, 2), true));
            grid2 = ((int[][])(new int[][]{new int[]{1, 1, 1}, new int[]{0, 0, 1}, new int[]{0, 1, 1}}));
            print_result(dijkstra(((int[][])(grid2)), new Point(0, 0), new Point(2, 2), false));
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

    static <T> T[] concat(T[] a, T[] b) {
        T[] out = java.util.Arrays.copyOf(a, a.length + b.length);
        System.arraycopy(b, 0, out, a.length, b.length);
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
