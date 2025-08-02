public class Main {
    static class Colour {
        int R;
        int G;
        int B;
        Colour(int R, int G, int B) {
            this.R = R;
            this.G = G;
            this.B = B;
        }
        @Override public String toString() {
            return String.format("{'R': %s, 'G': %s, 'B': %s}", String.valueOf(R), String.valueOf(G), String.valueOf(B));
        }
    }

    static class Bitmap {
        int width;
        int height;
        Colour[][] pixels;
        Bitmap(int width, int height, Colour[][] pixels) {
            this.width = width;
            this.height = height;
            this.pixels = pixels;
        }
        @Override public String toString() {
            return String.format("{'width': %s, 'height': %s, 'pixels': %s}", String.valueOf(width), String.valueOf(height), String.valueOf(pixels));
        }
    }


    static Bitmap newBitmap(int w, int h, Colour c) {
        Colour[][] rows = new Colour[][]{};
        int y = 0;
        while (y < h) {
            Colour[] row = new Colour[]{};
            int x = 0;
            while (x < w) {
                row = java.util.stream.Stream.concat(java.util.Arrays.stream(row), java.util.stream.Stream.of(c)).toArray(Colour[]::new);
                x = x + 1;
            }
            rows = appendObj(rows, row);
            y = y + 1;
        }
        return new Bitmap(w, h, rows);
    }

    static void setPixel(Bitmap b, int x, int y, Colour c) {
        Colour[][] rows_1 = b.pixels;
        Colour[] row_1 = rows_1[y];
row_1[x] = c;
rows_1[y] = row_1;
b.pixels = rows_1;
    }

    static void fillRect(Bitmap b, int x, int y, int w, int h, Colour c) {
        int yy = y;
        while (yy < y + h) {
            int xx = x;
            while (xx < x + w) {
                setPixel(b, xx, yy, c);
                xx = xx + 1;
            }
            yy = yy + 1;
        }
    }

    static String pad(int n, int width) {
        String s = _p(n);
        while (_runeLen(s) < width) {
            s = " " + s;
        }
        return s;
    }

    static String writePPMP3(Bitmap b) {
        int maxv = 0;
        int y_1 = 0;
        while (y_1 < b.height) {
            int x_1 = 0;
            while (x_1 < b.width) {
                Colour p = b.pixels[y_1][x_1];
                if (p.R > maxv) {
                    maxv = p.R;
                }
                if (p.G > maxv) {
                    maxv = p.G;
                }
                if (p.B > maxv) {
                    maxv = p.B;
                }
                x_1 = x_1 + 1;
            }
            y_1 = y_1 + 1;
        }
        String out = "P3\n# generated from Bitmap.writeppmp3\n" + _p(b.width) + " " + _p(b.height) + "\n" + _p(maxv) + "\n";
        int numsize = _runeLen(_p(maxv));
        y_1 = b.height - 1;
        while (y_1 >= 0) {
            String line = "";
            int x_2 = 0;
            while (x_2 < b.width) {
                Colour p_1 = b.pixels[y_1][x_2];
                line = line + "   " + String.valueOf(pad(p_1.R, numsize)) + " " + String.valueOf(pad(p_1.G, numsize)) + " " + String.valueOf(pad(p_1.B, numsize));
                x_2 = x_2 + 1;
            }
            out = out + line;
            if (y_1 > 0) {
                out = out + "\n";
            } else {
                out = out + "\n";
            }
            y_1 = y_1 - 1;
        }
        return out;
    }

    static void main() {
        Colour black = new Colour(0, 0, 0);
        Colour white = new Colour(255, 255, 255);
        Bitmap bm = newBitmap(4, 4, black);
        fillRect(bm, 1, 0, 1, 2, white);
        setPixel(bm, 3, 3, new Colour(127, 0, 63));
        String ppm = String.valueOf(writePPMP3(bm));
        System.out.println(ppm);
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

    static int _runeLen(String s) {
        return s.codePointCount(0, s.length());
    }

    static String _p(Object v) {
        return v != null ? String.valueOf(v) : "<nil>";
    }
}
