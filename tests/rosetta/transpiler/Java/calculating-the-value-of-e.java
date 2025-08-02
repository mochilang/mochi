public class Main {
    static double epsilon;
    static int factval;
    static double e;
    static int n_1;
    static double term;

    static double absf(double x) {
        if (x < 0.0) {
            return -x;
        }
        return x;
    }

    static double pow10(int n) {
        double r = 1.0;
        int i = 0;
        while (i < n) {
            r = r * 10.0;
            i = i + 1;
        }
        return r;
    }

    static String formatFloat(double f, int prec) {
        double scale = pow10(prec);
        double scaled = (f * scale) + 0.5;
        int n = (((Number)(scaled)).intValue());
        String digits = _p(n);
        while (_runeLen(digits) <= prec) {
            digits = "0" + digits;
        }
        String intPart = _substr(digits, 0, _runeLen(digits) - prec);
        String fracPart = _substr(digits, _runeLen(digits) - prec, _runeLen(digits));
        return intPart + "." + fracPart;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            epsilon = 1e-15;
            factval = 1;
            e = 2.0;
            n_1 = 2;
            term = 1.0;
            while (true) {
                factval = factval * n_1;
                n_1 = n_1 + 1;
                term = 1.0 / (((Number)(factval)).doubleValue());
                e = e + term;
                if (absf(term) < epsilon) {
                    break;
                }
            }
            System.out.println("e = " + String.valueOf(formatFloat(e, 15)));
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

    static String _p(Object v) {
        return v != null ? String.valueOf(v) : "<nil>";
    }
}
