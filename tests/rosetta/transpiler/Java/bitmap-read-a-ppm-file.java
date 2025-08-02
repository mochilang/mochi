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
        int w;
        int h;
        int max;
        Pixel[][] data;
        Bitmap(int w, int h, int max, Pixel[][] data) {
            this.w = w;
            this.h = h;
            this.max = max;
            this.data = data;
        }
        @Override public String toString() {
            return String.format("{'w': %s, 'h': %s, 'max': %s, 'data': %s}", String.valueOf(w), String.valueOf(h), String.valueOf(max), String.valueOf(data));
        }
    }

    static String ppmtxt;
    static Bitmap bm_1;
    static String out_3;

    static Bitmap newBitmap(int w, int h, int max) {
        Pixel[][] rows = new Pixel[][]{};
        int y = 0;
        while (y < h) {
            Pixel[] row = new Pixel[]{};
            int x = 0;
            while (x < w) {
                row = java.util.stream.Stream.concat(java.util.Arrays.stream(row), java.util.stream.Stream.of(new Pixel(0, 0, 0))).toArray(Pixel[]::new);
                x = x + 1;
            }
            rows = appendObj(rows, row);
            y = y + 1;
        }
        return new Bitmap(w, h, max, rows);
    }

    static void setPx(Bitmap b, int x, int y, Pixel p) {
        Pixel[][] rows_1 = b.data;
        Pixel[] row_1 = rows_1[y];
row_1[x] = p;
rows_1[y] = row_1;
b.data = rows_1;
    }

    static Pixel getPx(Bitmap b, int x, int y) {
        return b.data[y][x];
    }

    static String[] splitLines(String s) {
        String[] out = new String[]{};
        String cur = "";
        int i = 0;
        while (i < _runeLen(s)) {
            String ch = _substr(s, i, i + 1);
            if ((ch.equals("\n"))) {
                out = java.util.stream.Stream.concat(java.util.Arrays.stream(out), java.util.stream.Stream.of(cur)).toArray(String[]::new);
                cur = "";
            } else {
                cur = cur + ch;
            }
            i = i + 1;
        }
        out = java.util.stream.Stream.concat(java.util.Arrays.stream(out), java.util.stream.Stream.of(cur)).toArray(String[]::new);
        return out;
    }

    static String[] splitWS(String s) {
        String[] out_1 = new String[]{};
        String cur_1 = "";
        int i_1 = 0;
        while (i_1 < _runeLen(s)) {
            String ch_1 = _substr(s, i_1, i_1 + 1);
            if ((ch_1.equals(" ")) || (ch_1.equals("\t")) || (ch_1.equals("\r")) || (ch_1.equals("\n"))) {
                if (_runeLen(cur_1) > 0) {
                    out_1 = java.util.stream.Stream.concat(java.util.Arrays.stream(out_1), java.util.stream.Stream.of(cur_1)).toArray(String[]::new);
                    cur_1 = "";
                }
            } else {
                cur_1 = cur_1 + ch_1;
            }
            i_1 = i_1 + 1;
        }
        if (_runeLen(cur_1) > 0) {
            out_1 = java.util.stream.Stream.concat(java.util.Arrays.stream(out_1), java.util.stream.Stream.of(cur_1)).toArray(String[]::new);
        }
        return out_1;
    }

    static int parseIntStr(String str) {
        int i_2 = 0;
        boolean neg = false;
        if (_runeLen(str) > 0 && (str.substring(0, 1).equals("-"))) {
            neg = true;
            i_2 = 1;
        }
        int n = 0;
        java.util.Map<String,Integer> digits = ((java.util.Map<String,Integer>)(new java.util.LinkedHashMap<String, Integer>(java.util.Map.ofEntries(java.util.Map.entry("0", 0), java.util.Map.entry("1", 1), java.util.Map.entry("2", 2), java.util.Map.entry("3", 3), java.util.Map.entry("4", 4), java.util.Map.entry("5", 5), java.util.Map.entry("6", 6), java.util.Map.entry("7", 7), java.util.Map.entry("8", 8), java.util.Map.entry("9", 9)))));
        while (i_2 < _runeLen(str)) {
            n = n * 10 + (int)(((int)(digits).get(str.substring(i_2, i_2 + 1))));
            i_2 = i_2 + 1;
        }
        if (neg) {
            n = -n;
        }
        return n;
    }

    static String[] tokenize(String s) {
        String[] lines = splitLines(s);
        String[] toks = new String[]{};
        int i_3 = 0;
        while (i_3 < lines.length) {
            String line = lines[i_3];
            if (_runeLen(line) > 0 && (_substr(line, 0, 1).equals("#"))) {
                i_3 = i_3 + 1;
                continue;
            }
            String[] parts = splitWS(line);
            int j = 0;
            while (j < parts.length) {
                toks = java.util.stream.Stream.concat(java.util.Arrays.stream(toks), java.util.stream.Stream.of(parts[j])).toArray(String[]::new);
                j = j + 1;
            }
            i_3 = i_3 + 1;
        }
        return toks;
    }

    static Bitmap readP3(String text) {
        String[] toks_1 = tokenize(text);
        if (toks_1.length < 4) {
            return newBitmap(0, 0, 0);
        }
        if (!(toks_1[0].equals("P3"))) {
            return newBitmap(0, 0, 0);
        }
        int w = Integer.parseInt(toks_1[1]);
        int h = Integer.parseInt(toks_1[2]);
        int maxv = Integer.parseInt(toks_1[3]);
        int idx = 4;
        Bitmap bm = newBitmap(w, h, maxv);
        int y_1 = h - 1;
        while (y_1 >= 0) {
            int x_1 = 0;
            while (x_1 < w) {
                int r = Integer.parseInt(toks_1[idx]);
                int g = Integer.parseInt(toks_1[idx + 1]);
                int b = Integer.parseInt(toks_1[idx + 2]);
                setPx(bm, x_1, y_1, new Pixel(r, g, b));
                idx = idx + 3;
                x_1 = x_1 + 1;
            }
            y_1 = y_1 - 1;
        }
        return bm;
    }

    static void toGrey(Bitmap b) {
        int h_1 = b.h;
        int w_1 = b.w;
        int m = 0;
        int y_2 = 0;
        while (y_2 < h_1) {
            int x_2 = 0;
            while (x_2 < w_1) {
                Pixel p = getPx(b, x_2, y_2);
                int l = (p.R * 2126 + p.G * 7152 + p.B * 722) / 10000;
                if (l > b.max) {
                    l = b.max;
                }
                setPx(b, x_2, y_2, new Pixel(l, l, l));
                if (l > m) {
                    m = l;
                }
                x_2 = x_2 + 1;
            }
            y_2 = y_2 + 1;
        }
b.max = m;
    }

    static String pad(int n, int w) {
        String s = _p(n);
        while (_runeLen(s) < w) {
            s = " " + s;
        }
        return s;
    }

    static String writeP3(Bitmap b) {
        int h_2 = b.h;
        int w_2 = b.w;
        int max = b.max;
        int digits_1 = _runeLen(_p(max));
        String out_2 = "P3\n# generated from Bitmap.writeppmp3\n" + _p(w_2) + " " + _p(h_2) + "\n" + _p(max) + "\n";
        int y_3 = h_2 - 1;
        while (y_3 >= 0) {
            String line_1 = "";
            int x_3 = 0;
            while (x_3 < w_2) {
                Pixel p_1 = getPx(b, x_3, y_3);
                line_1 = line_1 + "   " + String.valueOf(pad(p_1.R, digits_1)) + " " + String.valueOf(pad(p_1.G, digits_1)) + " " + String.valueOf(pad(p_1.B, digits_1));
                x_3 = x_3 + 1;
            }
            out_2 = out_2 + line_1 + "\n";
            y_3 = y_3 - 1;
        }
        return out_2;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            ppmtxt = "P3\n" + "# feep.ppm\n" + "4 4\n" + "15\n" + " 0  0  0    0  0  0    0  0  0   15  0 15\n" + " 0  0  0    0 15  7    0  0  0    0  0  0\n" + " 0  0  0    0  0  0    0 15  7    0  0  0\n" + "15  0 15    0  0  0    0  0  0    0  0  0\n";
            System.out.println("Original Colour PPM file");
            System.out.println(ppmtxt);
            bm_1 = readP3(ppmtxt);
            System.out.println("Grey PPM:");
            toGrey(bm_1);
            out_3 = String.valueOf(writeP3(bm_1));
            System.out.println(out_3);
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

    static String _substr(String s, int i, int j) {
        int start = s.offsetByCodePoints(0, i);
        int end = s.offsetByCodePoints(0, j);
        return s.substring(start, end);
    }

    static String _p(Object v) {
        return v != null ? String.valueOf(v) : "<nil>";
    }
}
