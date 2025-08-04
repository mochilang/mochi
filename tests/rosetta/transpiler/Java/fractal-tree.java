public class Main {
    static double PI;
    static int width;
    static int height;
    static int depth;
    static double angle;
    static double length;
    static double frac;

    static double _mod(double x, double m) {
        return x - (((Number)(((Number)((x / m))).intValue())).doubleValue()) * m;
    }

    static double _sin(double x) {
        double y = _mod(x + PI, 2.0 * PI) - PI;
        double y2 = y * y;
        double y3 = y2 * y;
        double y5 = y3 * y2;
        double y7 = y5 * y2;
        return y - y3 / 6.0 + y5 / 120.0 - y7 / 5040.0;
    }

    static double _cos(double x) {
        double y_1 = _mod(x + PI, 2.0 * PI) - PI;
        double y2_1 = y_1 * y_1;
        double y4 = y2_1 * y2_1;
        double y6 = y4 * y2_1;
        return 1.0 - y2_1 / 2.0 + y4 / 24.0 - y6 / 720.0;
    }

    static String[][] clearGrid() {
        String[][] g = ((String[][])(new String[][]{}));
        int y_2 = 0;
        while (y_2 < height) {
            String[] row = ((String[])(new String[]{}));
            int x = 0;
            while (x < width) {
                row = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(row), java.util.stream.Stream.of(" ")).toArray(String[]::new)));
                x = x + 1;
            }
            g = ((String[][])(appendObj(g, row)));
            y_2 = y_2 + 1;
        }
        return g;
    }

    static void drawPoint(String[][] g, int x, int y) {
        if (x >= 0 && x < width && y >= 0 && y < height) {
            String[] row_1 = ((String[])(g[y]));
row_1[x] = "#";
g[y] = ((String[])(row_1));
        }
    }

    static void bresenham(int x0, int y0, int x1, int y1, String[][] g) {
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
            drawPoint(((String[][])(g)), x0, y0);
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

    static void ftree(String[][] g, double x, double y, double dist, double dir, int d) {
        double rad = dir * PI / 180.0;
        double x2 = x + dist * _sin(rad);
        double y2_2 = y - dist * _cos(rad);
        bresenham(((Number)(x)).intValue(), ((Number)(y)).intValue(), ((Number)(x2)).intValue(), ((Number)(y2_2)).intValue(), ((String[][])(g)));
        if (d > 0) {
            ftree(((String[][])(g)), x2, y2_2, dist * frac, dir - angle, d - 1);
            ftree(((String[][])(g)), x2, y2_2, dist * frac, dir + angle, d - 1);
        }
    }

    static String render(String[][] g) {
        String out = "";
        int y_3 = 0;
        while (y_3 < height) {
            String line = "";
            int x_1 = 0;
            while (x_1 < width) {
                line = line + g[y_3][x_1];
                x_1 = x_1 + 1;
            }
            out = out + line;
            if (y_3 < height - 1) {
                out = out + "\n";
            }
            y_3 = y_3 + 1;
        }
        return out;
    }

    static void main() {
        String[][] grid = ((String[][])(clearGrid()));
        ftree(((String[][])(grid)), ((Number)((width / 2))).doubleValue(), ((Number)((height - 1))).doubleValue(), length, 0.0, depth);
        System.out.println(render(((String[][])(grid))));
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            PI = 3.141592653589793;
            width = 80;
            height = 40;
            depth = 6;
            angle = 12.0;
            length = 12.0;
            frac = 0.8;
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
