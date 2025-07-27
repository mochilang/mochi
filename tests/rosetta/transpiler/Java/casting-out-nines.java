public class Main {
    static Data1[] testCases = new Data1[]{new Data1(10, "1", "100", new String[]{"1", "9", "45", "55", "99"}), new Data1(17, "10", "gg", new String[]{"3d", "d4", "gg"})};
    static class Data1 {
        int base;
        String begin;
        String end;
        String[] kaprekar;
        Data1(int base, String begin, String end, String[] kaprekar) {
            this.base = base;
            this.begin = begin;
            this.end = end;
            this.kaprekar = kaprekar;
        }
        @Override public String toString() {
            return String.format("{'base': %s, 'begin': '%s', 'end': '%s', 'kaprekar': %s}", String.valueOf(base), String.valueOf(begin), String.valueOf(end), String.valueOf(kaprekar));
        }
    }

    static int idx = 0;

    static int parseIntBase(String s, int base) {
        String digits = "0123456789abcdefghijklmnopqrstuvwxyz";
        int n = 0;
        int i = 0;
        while (i < _runeLen(s)) {
            int j = 0;
            int v = 0;
            while (j < _runeLen(digits)) {
                if ((_substr(digits, j, j + 1).equals(s.substring(i, i + 1)))) {
                    v = j;
                    break;
                }
                j = j + 1;
            }
            n = n * base + v;
            i = i + 1;
        }
        return n;
    }

    static String intToBase(int n, int base) {
        String digits = "0123456789abcdefghijklmnopqrstuvwxyz";
        if (n == 0) {
            return "0";
        }
        String out = "";
        int v = n;
        while (v > 0) {
            int d = Math.floorMod(v, base);
            out = digits.substring(d, d + 1) + out;
            v = v / base;
        }
        return out;
    }

    static String[] subset(int base, String begin, String end) {
        int b = parseIntBase(begin, base);
        int e = parseIntBase(end, base);
        String[] out = new String[]{};
        int k = b;
        while (k <= e) {
            String ks = String.valueOf(intToBase(k, base));
            int mod = base - 1;
            int r1 = Math.floorMod(parseIntBase(ks, base), mod);
            int r2 = Math.floorMod((parseIntBase(ks, base) * parseIntBase(ks, base)), mod);
            if (r1 == r2) {
                out = java.util.stream.Stream.concat(java.util.Arrays.stream(out), java.util.stream.Stream.of(ks)).toArray(String[]::new);
            }
            k = k + 1;
        }
        return out;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            while (idx < testCases.length) {
                Data1 tc = testCases[idx];
                System.out.println("\nTest case base = " + String.valueOf(tc.base) + ", begin = " + tc.begin + ", end = " + tc.end + ":");
                String[] s = subset(tc.base, tc.begin, tc.end);
                System.out.println("Subset:  " + String.valueOf(s));
                System.out.println("Kaprekar:" + String.valueOf(tc.kaprekar));
                int sx = 0;
                boolean valid = true;
                int i = 0;
                while (i < tc.kaprekar.length) {
                    String k = tc.kaprekar[i];
                    boolean found = false;
                    while (sx < s.length) {
                        if ((s[sx].equals(k))) {
                            found = true;
                            sx = sx + 1;
                            break;
                        }
                        sx = sx + 1;
                    }
                    if (!found) {
                        System.out.println("Fail:" + k + " not in subset");
                        valid = false;
                        break;
                    }
                    i = i + 1;
                }
                if (valid) {
                    System.out.println("Valid subset.");
                }
                idx = idx + 1;
            }
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

    static int _runeLen(String s) {
        return s.codePointCount(0, s.length());
    }

    static String _substr(String s, int i, int j) {
        int start = s.offsetByCodePoints(0, i);
        int end = s.offsetByCodePoints(0, j);
        return s.substring(start, end);
    }
}
