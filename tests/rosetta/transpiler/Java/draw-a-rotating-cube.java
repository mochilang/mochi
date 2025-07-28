public class Main {
    static double PI = 3.141592653589793;
    static double TWO_PI = 6.283185307179586;
    static class Point3 {
        double x;
        double y;
        double z;
        Point3(double x, double y, double z) {
            this.x = x;
            this.y = y;
            this.z = z;
        }
        @Override public String toString() {
            return String.format("{'x': %s, 'y': %s, 'z': %s}", String.valueOf(x), String.valueOf(y), String.valueOf(z));
        }
    }

    static class Point2 {
        int x;
        int y;
        Point2(int x, int y) {
            this.x = x;
            this.y = y;
        }
        @Override public String toString() {
            return String.format("{'x': %s, 'y': %s}", String.valueOf(x), String.valueOf(y));
        }
    }

    static Point3[] nodes = new Point3[]{new Point3(-1.0, -1.0, -1.0), new Point3(-1.0, -1.0, 1.0), new Point3(-1.0, 1.0, -1.0), new Point3(-1.0, 1.0, 1.0), new Point3(1.0, -1.0, -1.0), new Point3(1.0, -1.0, 1.0), new Point3(1.0, 1.0, -1.0), new Point3(1.0, 1.0, 1.0)};
    static int[][] edges = new int[][]{new int[]{0, 1}, new int[]{1, 3}, new int[]{3, 2}, new int[]{2, 0}, new int[]{4, 5}, new int[]{5, 7}, new int[]{7, 6}, new int[]{6, 4}, new int[]{0, 4}, new int[]{1, 5}, new int[]{2, 6}, new int[]{3, 7}};
    static int width = 40;
    static int height = 20;
    static double distance = 3.0;
    static double scale = 8.0;

    static double _mod(double x, double m) {
        return x - (((Number)(((Number)((x / m))).intValue())).doubleValue()) * m;
    }

    static double _sin(double x) {
        double y = _mod(x + PI, TWO_PI) - PI;
        double y2 = y * y;
        double y3 = y2 * y;
        double y5 = y3 * y2;
        double y7 = y5 * y2;
        return y - y3 / 6.0 + y5 / 120.0 - y7 / 5040.0;
    }

    static double _cos(double x) {
        double y = _mod(x + PI, TWO_PI) - PI;
        double y2 = y * y;
        double y4 = y2 * y2;
        double y6 = y4 * y2;
        return 1.0 - y2 / 2.0 + y4 / 24.0 - y6 / 720.0;
    }

    static Point3 rotate(Point3 p, double ax, double ay) {
        double sinx = _sin(ax);
        double cosx = _cos(ax);
        double siny = _sin(ay);
        double cosy = _cos(ay);
        double x1 = p.x;
        double y1 = p.y * cosx - p.z * sinx;
        double z1 = p.y * sinx + p.z * cosx;
        double x2 = x1 * cosy + z1 * siny;
        double z2 = -x1 * siny + z1 * cosy;
        return new Point3(x2, y1, z2);
    }

    static Point2 project(Point3 p) {
        double factor = scale / (p.z + distance);
        int x = ((Number)((p.x * factor))).intValue() + width / 2;
        int y = ((Number)((-p.y * factor))).intValue() + height / 2;
        return new Point2(x, y);
    }

    static String[][] clearGrid() {
        String[][] g = new String[][]{};
        int y = 0;
        while (y < height) {
            String[] row = new String[]{};
            int x = 0;
            while (x < width) {
                row = java.util.stream.Stream.concat(java.util.Arrays.stream(row), java.util.stream.Stream.of(" ")).toArray(String[]::new);
                x = x + 1;
            }
            g = appendObj(g, row);
            y = y + 1;
        }
        return g;
    }

    static void drawPoint(String[][] g, int x, int y, String ch) {
        if (x >= 0 && x < width && y >= 0 && y < height) {
            String[] row = g[y];
row[x] = ch;
g[y] = row;
        }
    }

    static void bresenham(int x0, int y0, int x1, int y1, String[][] g, String ch) {
        int dx = x1 - x0;
        if (dx < 0) {
            dx = -dx;
        }
        int dy = y1 - y0;
        if (dy < 0) {
            dy = -dy;
        }
        int sx = -1;
        if (x0 < x1) {
            sx = 1;
        }
        int sy = -1;
        if (y0 < y1) {
            sy = 1;
        }
        int err = dx - dy;
        while (true) {
            drawPoint(g, x0, y0, ch);
            if (x0 == x1 && y0 == y1) {
                break;
            }
            int e2 = 2 * err;
            if (e2 > (-dy)) {
                err = err - dy;
                x0 = x0 + sx;
            }
            if (e2 < dx) {
                err = err + dx;
                y0 = y0 + sy;
            }
        }
    }

    static String render(String[][] g) {
        String out = "";
        int y = 0;
        while (y < height) {
            String line = "";
            int x = 0;
            while (x < width) {
                line = line + g[y][x];
                x = x + 1;
            }
            out = out + line + "\n";
            y = y + 1;
        }
        return out;
    }

    static void main() {
        int f = 0;
        while (f < 10) {
            String[][] grid = clearGrid();
            Point2[] rot = new Point2[]{};
            int i = 0;
            double ay = (PI / 4.0) + (((Number)(f)).doubleValue()) * PI / 10.0;
            while (i < nodes.length) {
                Point3 p = rotate(nodes[i], PI / 4.0, ay);
                Point2 pp = project(p);
                rot = java.util.stream.Stream.concat(java.util.Arrays.stream(rot), java.util.stream.Stream.of(pp)).toArray(Point2[]::new);
                i = i + 1;
            }
            int e = 0;
            while (e < edges.length) {
                int a = edges[e][0];
                int b = edges[e][1];
                Point2 p1 = rot[a];
                Point2 p2 = rot[b];
                bresenham(p1.x, p1.y, p2.x, p2.y, grid, "#");
                e = e + 1;
            }
            System.out.println(render(grid));
            f = f + 1;
        }
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
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

    static <T> T[] appendObj(T[] arr, T v) {
        T[] out = java.util.Arrays.copyOf(arr, arr.length + 1);
        out[arr.length] = v;
        return out;
    }
}
