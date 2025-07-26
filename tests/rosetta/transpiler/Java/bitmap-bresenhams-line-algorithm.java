public class Main {
    static class Point {
        int x;
        int y;
        Point(int x, int y) {
            this.x = x;
            this.y = y;
        }
        @Override public String toString() {
            return String.format("{'x': %s, 'y': %s}", String.valueOf(x), String.valueOf(y));
        }
    }


    static int absi(int x) {
        if (x < 0) {
            return -x;
        }
        return x;
    }

    static Point[] bresenham(int x0, int y0, int x1, int y1) {
        int dx = absi(x1 - x0);
        int dy = absi(y1 - y0);
        int sx = -1;
        if (x0 < x1) {
            sx = 1;
        }
        int sy = -1;
        if (y0 < y1) {
            sy = 1;
        }
        int err = dx - dy;
        Point[] pts = new Point[]{};
        while (true) {
            pts = java.util.stream.Stream.concat(java.util.Arrays.stream(pts), java.util.stream.Stream.of(new Point(x0, y0))).toArray(Point[]::new);
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
        return pts;
    }

    static void main() {
        Point[] pts = bresenham(0, 0, 6, 4);
        int i = 0;
        while (i < pts.length) {
            Point p = pts[i];
            System.out.println("(" + String.valueOf(p.x) + "," + String.valueOf(p.y) + ")");
            i = i + 1;
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
}
