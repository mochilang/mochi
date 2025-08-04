public class Main {
    static int limit;

    static boolean isPrime(int n) {
        if (n < 2) {
            return false;
        }
        if (Math.floorMod(n, 2) == 0) {
            return n == 2;
        }
        if (Math.floorMod(n, 3) == 0) {
            return n == 3;
        }
        int d = 5;
        while (d * d <= n) {
            if (Math.floorMod(n, d) == 0) {
                return false;
            }
            d = d + 2;
            if (Math.floorMod(n, d) == 0) {
                return false;
            }
            d = d + 4;
        }
        return true;
    }

    static String commatize(int n) {
        String s = _p(n);
        int i = _runeLen(s) - 3;
        while (i >= 1) {
            s = s.substring(0, i) + "," + s.substring(i, _runeLen(s));
            i = i - 3;
        }
        return s;
    }

    static String padLeft(String s, int w) {
        String out = s;
        while (_runeLen(out) < w) {
            out = " " + out;
        }
        return out;
    }

    static String padRight(String s, int w) {
        String out_1 = s;
        while (_runeLen(out_1) < w) {
            out_1 = out_1 + " ";
        }
        return out_1;
    }

    static void main() {
        int i_1 = limit;
        int n = 0;
        while (n < limit) {
            if (((Boolean)(isPrime(i_1)))) {
                n = n + 1;
                String nStr = String.valueOf(padRight(_p(n), 2));
                String pStr = String.valueOf(padLeft(String.valueOf(commatize(i_1)), 19));
                System.out.println("n = " + nStr + "  " + pStr);
                i_1 = i_1 + i_1 - 1;
            }
            i_1 = i_1 + 1;
        }
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            limit = 42;
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

    static int _runeLen(String s) {
        return s.codePointCount(0, s.length());
    }

    static String _p(Object v) {
        return v != null ? String.valueOf(v) : "<nil>";
    }
}
