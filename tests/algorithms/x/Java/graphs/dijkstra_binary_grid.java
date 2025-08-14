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

    static long[][] grid1 = new long[0][];
    static long[][] grid2 = new long[0][];

    static String key(Point p) {
        return _p(p.x) + "," + _p(p.y);
    }

    static String path_to_string(Point[] path) {
        String s = "[";
        long i_1 = 0L;
        while ((long)(i_1) < (long)(path.length)) {
            Point pt_1 = path[(int)((long)(i_1))];
            s = s + "(" + _p(pt_1.x) + ", " + _p(pt_1.y) + ")";
            if ((long)(i_1) < (long)((long)(path.length) - 1L)) {
                s = s + ", ";
            }
            i_1 = (long)((long)(i_1) + 1L);
        }
        s = s + "]";
        return s;
    }

    static Result dijkstra(long[][] grid, Point source, Point destination, boolean allow_diagonal) {
        long rows = (long)(grid.length);
        long cols_1 = (long)(grid[(int)((long)(0))].length);
        long[] dx_1 = ((long[])(new long[]{-1, 1, 0, 0}));
        long[] dy_1 = ((long[])(new long[]{0, 0, -1, 1}));
        if (allow_diagonal) {
            dx_1 = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(dx_1), java.util.Arrays.stream(new long[]{-1, -1, 1, 1})).toArray()));
            dy_1 = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(dy_1), java.util.Arrays.stream(new long[]{-1, 1, -1, 1})).toArray()));
        }
        double INF_1 = (double)(1000000000000.0);
        Point[] queue_1 = ((Point[])(new Point[]{source}));
        long front_1 = 0L;
        java.util.Map<String,Double> dist_map_1 = ((java.util.Map<String,Double>)(new java.util.LinkedHashMap<String, Double>(java.util.Map.ofEntries(java.util.Map.entry(String.valueOf(key(source)), (double)(0.0))))));
        java.util.Map<String,Point> prev_1 = ((java.util.Map<String,Point>)(new java.util.LinkedHashMap<String, Point>()));
        while ((long)(front_1) < (long)(queue_1.length)) {
            Point current_1 = queue_1[(int)((long)(front_1))];
            front_1 = (long)((long)(front_1) + 1L);
            String cur_key_1 = String.valueOf(key(current_1));
            if ((long)(current_1.x) == (long)(destination.x) && (long)(current_1.y) == (long)(destination.y)) {
                break;
            }
            long i_3 = 0L;
            while ((long)(i_3) < (long)(dx_1.length)) {
                long nx_1 = (long)((long)(current_1.x) + (long)(dx_1[(int)((long)(i_3))]));
                long ny_1 = (long)((long)(current_1.y) + (long)(dy_1[(int)((long)(i_3))]));
                if ((long)(nx_1) >= 0L && (long)(nx_1) < (long)(rows) && (long)(ny_1) >= 0L && (long)(ny_1) < (long)(cols_1)) {
                    if ((long)(grid[(int)((long)(nx_1))][(int)((long)(ny_1))]) == 1L) {
                        String n_key_1 = _p(nx_1) + "," + _p(ny_1);
                        if (!(dist_map_1.containsKey(n_key_1))) {
dist_map_1.put(n_key_1, (double)((double)(((double)(dist_map_1).getOrDefault(cur_key_1, 0.0))) + (double)(1.0)));
prev_1.put(n_key_1, current_1);
                            queue_1 = ((Point[])(java.util.stream.Stream.concat(java.util.Arrays.stream(queue_1), java.util.stream.Stream.of(new Point(nx_1, ny_1))).toArray(Point[]::new)));
                        }
                    }
                }
                i_3 = (long)((long)(i_3) + 1L);
            }
        }
        String dest_key_1 = String.valueOf(key(destination));
        if (dist_map_1.containsKey(dest_key_1)) {
            Point[] path_rev_1 = ((Point[])(new Point[]{destination}));
            String step_key_1 = dest_key_1;
            Point step_pt_1 = destination;
            while (!(step_key_1.equals(key(source)))) {
                step_pt_1 = (Point)(((Point)(prev_1).get(step_key_1)));
                step_key_1 = String.valueOf(key(step_pt_1));
                path_rev_1 = ((Point[])(java.util.stream.Stream.concat(java.util.Arrays.stream(path_rev_1), java.util.stream.Stream.of(step_pt_1)).toArray(Point[]::new)));
            }
            Point[] path_1 = ((Point[])(new Point[]{}));
            long k_1 = (long)((long)(path_rev_1.length) - 1L);
            while ((long)(k_1) >= 0L) {
                path_1 = ((Point[])(java.util.stream.Stream.concat(java.util.Arrays.stream(path_1), java.util.stream.Stream.of(path_rev_1[(int)((long)(k_1))])).toArray(Point[]::new)));
                k_1 = (long)((long)(k_1) - 1L);
            }
            return new Result(((double)(dist_map_1).getOrDefault(dest_key_1, 0.0)), path_1);
        }
        return new Result(INF_1, new Point[]{});
    }

    static void print_result(Result res) {
        System.out.println(_p(res.distance) + ", " + String.valueOf(path_to_string(((Point[])(res.path)))));
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            grid1 = ((long[][])(new long[][]{new long[]{1, 1, 1}, new long[]{0, 1, 0}, new long[]{0, 1, 1}}));
            print_result(dijkstra(((long[][])(grid1)), new Point(0, 0), new Point(2, 2), false));
            print_result(dijkstra(((long[][])(grid1)), new Point(0, 0), new Point(2, 2), true));
            grid2 = ((long[][])(new long[][]{new long[]{1, 1, 1}, new long[]{0, 0, 1}, new long[]{0, 1, 1}}));
            print_result(dijkstra(((long[][])(grid2)), new Point(0, 0), new Point(2, 2), false));
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
