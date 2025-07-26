public class Main {
    static int b3Seg = 30;
    static class Pixel {
        int r;
        int g;
        int b;
        Pixel(int r, int g, int b) {
            this.r = r;
            this.g = g;
            this.b = b;
        }
        @Override public String toString() {
            return String.format("{'r': %s, 'g': %s, 'b': %s}", String.valueOf(r), String.valueOf(g), String.valueOf(b));
        }
    }

    static java.util.Map<String,Object> b = newBitmap(400, 300);

    static Pixel pixelFromRgb(int rgb) {
        int r = ((Number)((Math.floorMod((rgb / 65536), 256)))).intValue();
        int g = ((Number)((Math.floorMod((rgb / 256), 256)))).intValue();
        int b = ((Number)((Math.floorMod(rgb, 256)))).intValue();
        return new Pixel(r, g, b);
    }

    static java.util.Map<String,Object> newBitmap(int cols, int rows) {
        Pixel[][] d = new Pixel[][]{};
        int y = 0;
        while (y < rows) {
            Pixel[] row = new Pixel[]{};
            int x = 0;
            while (x < cols) {
                row = java.util.stream.Stream.concat(java.util.Arrays.stream(row), java.util.stream.Stream.of(new Pixel(0, 0, 0))).toArray(Pixel[]::new);
                x = x + 1;
            }
            d = appendObj(d, row);
            y = y + 1;
        }
        return new java.util.LinkedHashMap<String, Object>(java.util.Map.ofEntries(java.util.Map.entry("cols", cols), java.util.Map.entry("rows", rows), java.util.Map.entry("data", d)));
    }

    static void setPx(java.util.Map<String,Object> b, int x, int y, Pixel p) {
        int cols = (int)(((int)b.getOrDefault("cols", 0)));
        int rows = (int)(((int)b.getOrDefault("rows", 0)));
        if (x >= 0 && x < cols && y >= 0 && y < rows) {
((Object)b.get("data"))[y][x] = p;
        }
    }

    static void fill(java.util.Map<String,Object> b, Pixel p) {
        int cols = (int)(((int)b.getOrDefault("cols", 0)));
        int rows = (int)(((int)b.getOrDefault("rows", 0)));
        int y = 0;
        while (y < rows) {
            int x = 0;
            while (x < cols) {
((Object)b.get("data"))[y][x] = p;
                x = x + 1;
            }
            y = y + 1;
        }
    }

    static void fillRgb(java.util.Map<String,Object> b, int rgb) {
        fill(b, pixelFromRgb(rgb));
    }

    static void line(java.util.Map<String,Object> b, int x0, int y0, int x1, int y1, Pixel p) {
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
            setPx(b, x0, y0, p);
            if (x0 == x1 && y0 == y1) {
                break;
            }
            int e2 = 2 * err;
            if (e2 > (0 - dy)) {
                err = err - dy;
                x0 = x0 + sx;
            }
            if (e2 < dx) {
                err = err + dx;
                y0 = y0 + sy;
            }
        }
    }

    static void bezier3(java.util.Map<String,Object> b, int x1, int y1, int x2, int y2, int x3, int y3, int x4, int y4, Pixel p) {
        int[] px = new int[]{};
        int[] py = new int[]{};
        int i = 0;
        while (i <= b3Seg) {
            px = java.util.stream.IntStream.concat(java.util.Arrays.stream(px), java.util.stream.IntStream.of(0)).toArray();
            py = java.util.stream.IntStream.concat(java.util.Arrays.stream(py), java.util.stream.IntStream.of(0)).toArray();
            i = i + 1;
        }
        double fx1 = ((Number)(x1)).doubleValue();
        double fy1 = ((Number)(y1)).doubleValue();
        double fx2 = ((Number)(x2)).doubleValue();
        double fy2 = ((Number)(y2)).doubleValue();
        double fx3 = ((Number)(x3)).doubleValue();
        double fy3 = ((Number)(y3)).doubleValue();
        double fx4 = ((Number)(x4)).doubleValue();
        double fy4 = ((Number)(y4)).doubleValue();
        i = 0;
        while (i <= b3Seg) {
            double d = (((Number)(i)).doubleValue()) / (((Number)(b3Seg)).doubleValue());
            double a = 1.0 - d;
            double bcoef = a * a;
            double ccoef = d * d;
            double a2 = a * bcoef;
            double b2 = 3.0 * bcoef * d;
            double c2 = 3.0 * a * ccoef;
            double d2 = ccoef * d;
px[i] = ((Number)((a2 * fx1 + b2 * fx2 + c2 * fx3 + d2 * fx4))).intValue();
py[i] = ((Number)((a2 * fy1 + b2 * fy2 + c2 * fy3 + d2 * fy4))).intValue();
            i = i + 1;
        }
        int x0 = px[0];
        int y0 = py[0];
        i = 1;
        while (i <= b3Seg) {
            int x = px[i];
            int y = py[i];
            line(b, x0, y0, x, y, p);
            x0 = x;
            y0 = y;
            i = i + 1;
        }
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            fillRgb(b, 16773055);
            bezier3(b, 20, 200, 700, 50, -300, 50, 380, 150, pixelFromRgb(4165615));
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
