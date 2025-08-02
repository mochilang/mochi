public class Main {
    static class Pixel {
        int R;
        int G;
        int B;
        Pixel(int R, int G, int B) {
            this.R = R;
            this.G = G;
            this.B = B;
        }
        @Override public String toString() {
            return String.format("{'R': %s, 'G': %s, 'B': %s}", String.valueOf(R), String.valueOf(G), String.valueOf(B));
        }
    }

    static class Bitmap {
        int cols;
        int rows;
        Pixel[][] px;
        Bitmap(int cols, int rows, Pixel[][] px) {
            this.cols = cols;
            this.rows = rows;
            this.px = px;
        }
        @Override public String toString() {
            return String.format("{'cols': %s, 'rows': %s, 'px': %s}", String.valueOf(cols), String.valueOf(rows), String.valueOf(px));
        }
    }


    static Pixel pixelFromRgb(int c) {
        int r = Math.floorMod((((Number)((c / 65536))).intValue()), 256);
        int g = Math.floorMod((((Number)((c / 256))).intValue()), 256);
        int b = Math.floorMod(c, 256);
        return new Pixel(r, g, b);
    }

    static int rgbFromPixel(Pixel p) {
        return p.R * 65536 + p.G * 256 + p.B;
    }

    static Bitmap NewBitmap(int x, int y) {
        Pixel[][] data = new Pixel[][]{};
        int row = 0;
        while (row < y) {
            Pixel[] r_1 = new Pixel[]{};
            int col = 0;
            while (col < x) {
                r_1 = java.util.stream.Stream.concat(java.util.Arrays.stream(r_1), java.util.stream.Stream.of(new Pixel(0, 0, 0))).toArray(Pixel[]::new);
                col = col + 1;
            }
            data = appendObj(data, r_1);
            row = row + 1;
        }
        return new Bitmap(x, y, data);
    }

    static void FillRgb(Bitmap b, int c) {
        int y = 0;
        Pixel p = pixelFromRgb(c);
        while (y < b.rows) {
            int x = 0;
            while (x < b.cols) {
                Pixel[][] px = b.px;
                Pixel[] row_1 = px[y];
row_1[x] = p;
px[y] = row_1;
b.px = px;
                x = x + 1;
            }
            y = y + 1;
        }
    }

    static boolean SetPxRgb(Bitmap b, int x, int y, int c) {
        if (x < 0 || x >= b.cols || y < 0 || y >= b.rows) {
            return false;
        }
        Pixel[][] px_1 = b.px;
        Pixel[] row_2 = px_1[y];
row_2[x] = pixelFromRgb(c);
px_1[y] = row_2;
b.px = px_1;
        return true;
    }

    static int nextRand(int seed) {
        return Math.floorMod((seed * 1664525 + 1013904223), (int)2147483648L);
    }

    static void main() {
        Bitmap bm = NewBitmap(400, 300);
        FillRgb(bm, 12615744);
        int seed = _now();
        int i = 0;
        while (i < 2000) {
            seed = nextRand(seed);
            int x_1 = Math.floorMod(seed, 400);
            seed = nextRand(seed);
            int y_1 = Math.floorMod(seed, 300);
            SetPxRgb(bm, x_1, y_1, 8405024);
            i = i + 1;
        }
        int x_2 = 0;
        while (x_2 < 400) {
            int y_2 = 240;
            while (y_2 < 245) {
                SetPxRgb(bm, x_2, y_2, 8405024);
                y_2 = y_2 + 1;
            }
            y_2 = 260;
            while (y_2 < 265) {
                SetPxRgb(bm, x_2, y_2, 8405024);
                y_2 = y_2 + 1;
            }
            x_2 = x_2 + 1;
        }
        int y_3 = 0;
        while (y_3 < 300) {
            int x_3 = 80;
            while (x_3 < 85) {
                SetPxRgb(bm, x_3, y_3, 8405024);
                x_3 = x_3 + 1;
            }
            x_3 = 95;
            while (x_3 < 100) {
                SetPxRgb(bm, x_3, y_3, 8405024);
                x_3 = x_3 + 1;
            }
            y_3 = y_3 + 1;
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
