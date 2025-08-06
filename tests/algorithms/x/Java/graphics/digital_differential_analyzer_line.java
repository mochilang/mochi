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


    static int abs_int(int n) {
        if (n < 0) {
            return -n;
        }
        return n;
    }

    static int round_int(double x) {
        return ((Number)((x + 0.5))).intValue();
    }

    static Point[] digital_differential_analyzer_line(Point p1, Point p2) {
        int dx = p2.x - p1.x;
        int dy = p2.y - p1.y;
        int abs_dx = abs_int(dx);
        int abs_dy = abs_int(dy);
        int steps = abs_dx > abs_dy ? abs_dx : abs_dy;
        double x_increment = (((Number)(dx)).doubleValue()) / (((Number)(steps)).doubleValue());
        double y_increment = (((Number)(dy)).doubleValue()) / (((Number)(steps)).doubleValue());
        Point[] coordinates = ((Point[])(new Point[]{}));
        double x = ((Number)(p1.x)).doubleValue();
        double y = ((Number)(p1.y)).doubleValue();
        int i = 0;
        while (i < steps) {
            x = x + x_increment;
            y = y + y_increment;
            Point point = new Point(round_int(x), round_int(y));
            coordinates = ((Point[])(java.util.stream.Stream.concat(java.util.Arrays.stream(coordinates), java.util.stream.Stream.of(point)).toArray(Point[]::new)));
            i = i + 1;
        }
        return coordinates;
    }

    static void main() {
        Point[] result = ((Point[])(digital_differential_analyzer_line(new Point(1, 1), new Point(4, 4))));
        System.out.println(java.util.Arrays.toString(result));
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
