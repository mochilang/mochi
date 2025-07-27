public class Main {

    static int mod(int n, int m) {
        return Math.floorMod(((Math.floorMod(n, m)) + m), m);
    }

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

    static String pad(int n, int width) {
        String s = String.valueOf(n);
        while (_runeLen(s) < width) {
            s = " " + s;
        }
        return s;
    }

    static void carmichael(int p1) {
        for (int h3 = 2; h3 < p1; h3++) {
            for (int d = 1; d < (h3 + p1); d++) {
                if (Math.floorMod(((h3 + p1) * (p1 - 1)), d) == 0 && mod(-p1 * p1, h3) == Math.floorMod(d, h3)) {
                    int p2 = 1 + ((p1 - 1) * (h3 + p1) / d);
                    if (!(Boolean)isPrime(p2)) {
                        continue;
                    }
                    int p3 = 1 + (p1 * p2 / h3);
                    if (!(Boolean)isPrime(p3)) {
                        continue;
                    }
                    if (Math.floorMod((p2 * p3), (p1 - 1)) != 1) {
                        continue;
                    }
                    int c = p1 * p2 * p3;
                    System.out.println(String.valueOf(pad(p1, 2)) + "   " + String.valueOf(pad(p2, 4)) + "   " + String.valueOf(pad(p3, 5)) + "     " + String.valueOf(c));
                }
            }
        }
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            System.out.println("The following are Carmichael munbers for p1 <= 61:\n");
            System.out.println("p1     p2      p3     product");
            System.out.println("==     ==      ==     =======");
            for (int p1 = 2; p1 < 62; p1++) {
                if (isPrime(p1)) {
                    carmichael(p1);
                }
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
}
