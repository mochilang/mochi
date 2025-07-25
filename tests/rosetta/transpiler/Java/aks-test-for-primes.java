public class Main {

    static String poly(int p) {
        String s = "";
        int coef = 1;
        int i = p;
        if (coef != 1) {
            s = String.valueOf(s + String.valueOf(coef));
        }
        while (i > 0) {
            s = String.valueOf(s + "x");
            if (i != 1) {
                s = String.valueOf(String.valueOf(s + "^") + String.valueOf(i));
            }
            coef = ((Number)((coef * i / (p - i + 1)))).intValue();
            int d = coef;
            if ((p - (i - 1)) % 2 == 1) {
                d = -d;
            }
            if (d < 0) {
                s = String.valueOf(String.valueOf(s + " - ") + String.valueOf(-d));
            } else {
                s = String.valueOf(String.valueOf(s + " + ") + String.valueOf(d));
            }
            i = i - 1;
        }
        if ((s.equals(""))) {
            s = "1";
        }
        return s;
    }

    static boolean aks(int n) {
        if (n < 2) {
            return false;
        }
        int c = n;
        int i = 1;
        while (i < n) {
            if (c % n != 0) {
                return false;
            }
            c = ((Number)((c * (n - i) / (i + 1)))).intValue();
            i = i + 1;
        }
        return true;
    }

    static void main() {
        int p = 0;
        while (p <= 7) {
            System.out.println(String.valueOf(p) + ":  " + poly(p));
            p = p + 1;
        }
        boolean first = true;
        p = 2;
        String line = "";
        while (p < 50) {
            if (aks(p)) {
                if (first) {
                    line = String.valueOf(line + String.valueOf(p));
                    first = false;
                } else {
                    line = String.valueOf(String.valueOf(line + " ") + String.valueOf(p));
                }
            }
            p = p + 1;
        }
        System.out.println(line);
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
        return rt.totalMemory() - rt.freeMemory();
    }
}
