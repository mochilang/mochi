public class Main {

    static double floorf(double x) {
        int y = ((Number)(x)).intValue();
        return ((Number)(y)).doubleValue();
    }

    static int indexOf(String s, String ch) {
        int i = 0;
        while (i < _runeLen(s)) {
            if ((_substr(s, i, i + 1).equals(ch))) {
                return i;
            }
            i = i + 1;
        }
        return 0 - 1;
    }

    static String fmt8(double x) {
        double y_1 = floorf(x * 100000000.0 + 0.5) / 100000000.0;
        String s = String.valueOf(y_1);
        int dot = ((Number)(s.indexOf("."))).intValue();
        if (dot == 0 - 1) {
            s = s + ".00000000";
        } else {
            int decs = _runeLen(s) - dot - 1;
            while (decs < 8) {
                s = s + "0";
                decs = decs + 1;
            }
        }
        return s;
    }

    static String pad2(int x) {
        String s_1 = String.valueOf(x);
        if (_runeLen(s_1) < 2) {
            s_1 = " " + s_1;
        }
        return s_1;
    }

    static void main() {
        int maxIt = 13;
        int maxItJ = 10;
        double a1 = 1.0;
        double a2 = 0.0;
        double d1 = 3.2;
        System.out.println(" i       d");
        int i_1 = 2;
        while (i_1 <= maxIt) {
            double a = a1 + (a1 - a2) / d1;
            int j = 1;
            while (j <= maxItJ) {
                double x = 0.0;
                double y_2 = 0.0;
                int k = 1;
                int limit = pow_int(2, i_1);
                while (k <= limit) {
                    y_2 = 1.0 - 2.0 * y_2 * x;
                    x = a - x * x;
                    k = k + 1;
                }
                a = a - x / y_2;
                j = j + 1;
            }
            double d = (a1 - a2) / (a - a1);
            System.out.println(String.valueOf(pad2(i_1)) + "    " + String.valueOf(fmt8(d)));
            d1 = d;
            a2 = a1;
            a1 = a;
            i_1 = i_1 + 1;
        }
    }

    static int pow_int(int base, int exp) {
        int r = 1;
        int b = base;
        int e = exp;
        while (e > 0) {
            if (Math.floorMod(e, 2) == 1) {
                r = r * b;
            }
            b = b * b;
            e = ((Number)((e / 2))).intValue();
        }
        return r;
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

    static int _runeLen(String s) {
        return s.codePointCount(0, s.length());
    }

    static String _substr(String s, int i, int j) {
        int start = s.offsetByCodePoints(0, i);
        int end = s.offsetByCodePoints(0, j);
        return s.substring(start, end);
    }
}
