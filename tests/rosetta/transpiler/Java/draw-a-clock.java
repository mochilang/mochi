public class Main {
    static int t = _now() / 1000000000;
    static int sec = Math.floorMod(t, 60);
    static int mins = t / 60;
    static int min = Math.floorMod(mins, 60);
    static int hour = Math.floorMod((mins / 60), 24);
    static String xs = "";
    static int i = 0;
    static String out = "";
    static int j = 0;

    static int pow2(int exp) {
        int r = 1;
        int i = 0;
        while (i < exp) {
            r = r * 2;
            i = i + 1;
        }
        return r;
    }

    static String bin(int n, int digits) {
        String s = "";
        int i = digits - 1;
        while (i >= 0) {
            int p = pow2(i);
            if (n >= p) {
                s = s + "x";
                n = n - p;
            } else {
                s = s + " ";
            }
            if (i > 0) {
                s = s + "|";
            }
            i = i - 1;
        }
        return s;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            System.out.println(bin(hour, 8));
            System.out.println("");
            System.out.println(bin(min, 8));
            System.out.println("");
            while (i < sec) {
                xs = xs + "x";
                i = i + 1;
            }
            while (j < _runeLen(xs)) {
                out = out + _substr(xs, j, j + 1);
                if (Math.floorMod((j + 1), 5) == 0 && j + 1 < _runeLen(xs)) {
                    out = out + "|";
                }
                j = j + 1;
            }
            System.out.println(out);
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
