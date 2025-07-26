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
            Pixel[] r = new Pixel[]{};
            int col = 0;
            while (col < x) {
                r = java.util.stream.Stream.concat(java.util.Arrays.stream(r), java.util.stream.Stream.of(new Pixel(0, 0, 0))).toArray(Pixel[]::new);
                col = col + 1;
            }
            data = appendObj(data, r);
            row = row + 1;
        }
        return new Bitmap(x, y, data);
    }

    static java.util.Map<String,Integer> Extent(Bitmap b) {
        return new java.util.LinkedHashMap<String, Integer>(java.util.Map.ofEntries(java.util.Map.entry("cols", b.cols), java.util.Map.entry("rows", b.rows)));
    }

    static void Fill(Bitmap b, Pixel p) {
        int y = 0;
        while (y < b.rows) {
            int x = 0;
            while (x < b.cols) {
                Pixel[][] px = b.px;
                Pixel[] row = px[y];
row[x] = p;
px[y] = row;
b.px = px;
                x = x + 1;
            }
            y = y + 1;
        }
    }

    static void FillRgb(Bitmap b, int c) {
        Fill(b, pixelFromRgb(c));
    }

    static boolean SetPx(Bitmap b, int x, int y, Pixel p) {
        if (x < 0 || x >= b.cols || y < 0 || y >= b.rows) {
            return false;
        }
        Pixel[][] px = b.px;
        Pixel[] row = px[y];
row[x] = p;
px[y] = row;
b.px = px;
        return true;
    }

    static boolean SetPxRgb(Bitmap b, int x, int y, int c) {
        return SetPx(b, x, y, pixelFromRgb(c));
    }

    static java.util.Map<String,Object> GetPx(Bitmap b, int x, int y) {
        if (x < 0 || x >= b.cols || y < 0 || y >= b.rows) {
            return new java.util.LinkedHashMap<String, Object>(java.util.Map.ofEntries(java.util.Map.entry("ok", false)));
        }
        Pixel[] row = b.px[y];
        return new java.util.LinkedHashMap<String, Object>(java.util.Map.ofEntries(java.util.Map.entry("ok", true), java.util.Map.entry("pixel", row[x])));
    }

    static java.util.Map<String,Object> GetPxRgb(Bitmap b, int x, int y) {
        java.util.Map<String,Object> r = GetPx(b, x, y);
        if (!((boolean) (r.get("ok")))) {
            return new java.util.LinkedHashMap<String, Object>(java.util.Map.ofEntries(java.util.Map.entry("ok", false)));
        }
        return new java.util.LinkedHashMap<String, Object>(java.util.Map.ofEntries(java.util.Map.entry("ok", true), java.util.Map.entry("rgb", rgbFromPixel((Pixel)(((Pixel) (r.get("pixel"))))))));
    }

    static int ppmSize(Bitmap b) {
        String header = "P6\n# Creator: Rosetta Code http://rosettacode.org/\n" + String.valueOf(b.cols) + " " + String.valueOf(b.rows) + "\n255\n";
        return header.length() + 3 * b.cols * b.rows;
    }

    static String pixelStr(Pixel p) {
        return "{" + String.valueOf(p.R) + " " + String.valueOf(p.G) + " " + String.valueOf(p.B) + "}";
    }

    static void main() {
        Bitmap bm = NewBitmap(300, 240);
        FillRgb(bm, 16711680);
        SetPxRgb(bm, 10, 20, 255);
        SetPxRgb(bm, 20, 30, 0);
        SetPxRgb(bm, 30, 40, 1056816);
        java.util.Map<String,Object> c1 = GetPx(bm, 0, 0);
        java.util.Map<String,Object> c2 = GetPx(bm, 10, 20);
        java.util.Map<String,Object> c3 = GetPx(bm, 30, 40);
        System.out.println("Image size: " + String.valueOf(bm.cols) + " × " + String.valueOf(bm.rows));
        System.out.println(String.valueOf(ppmSize(bm)) + " bytes when encoded as PPM.");
        if (((boolean) (c1.get("ok")))) {
            System.out.println("Pixel at (0,0) is " + String.valueOf(pixelStr((Pixel)(((Pixel) (c1.get("pixel")))))));
        }
        if (((boolean) (c2.get("ok")))) {
            System.out.println("Pixel at (10,20) is " + String.valueOf(pixelStr((Pixel)(((Pixel) (c2.get("pixel")))))));
        }
        if (((boolean) (c3.get("ok")))) {
            Pixel p = (Pixel)(((Pixel) (c3.get("pixel"))));
            int r16 = p.R * 257;
            int g16 = p.G * 257;
            int b16 = p.B * 257;
            System.out.println("Pixel at (30,40) has R=" + String.valueOf(r16) + ", G=" + String.valueOf(g16) + ", B=" + String.valueOf(b16));
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
