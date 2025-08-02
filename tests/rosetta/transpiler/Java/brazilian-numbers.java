public class Main {

    static boolean sameDigits(int n, int b) {
        int f = Math.floorMod(n, b);
        n = ((Number)((n / b))).intValue();
        while (n > 0) {
            if (Math.floorMod(n, b) != f) {
                return false;
            }
            n = ((Number)((n / b))).intValue();
        }
        return true;
    }

    static boolean isBrazilian(int n) {
        if (n < 7) {
            return false;
        }
        if (Math.floorMod(n, 2) == 0 && n >= 8) {
            return true;
        }
        int b = 2;
        while (b < n - 1) {
            if (((Boolean)(sameDigits(n, b)))) {
                return true;
            }
            b = b + 1;
        }
        return false;
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

    static void main() {
        String[] kinds = new String[]{" ", " odd ", " prime "};
        for (String kind : kinds) {
            System.out.println("First 20" + kind + "Brazilian numbers:");
            int c = 0;
            int n = 7;
            while (true) {
                if (((Boolean)(isBrazilian(n)))) {
                    System.out.println(_p(n) + " ");
                    c = c + 1;
                    if (c == 20) {
                        System.out.println("\n");
                        break;
                    }
                }
                if ((kind.equals(" "))) {
                    n = n + 1;
                } else                 if ((kind.equals(" odd "))) {
                    n = n + 2;
                } else {
                    while (true) {
                        n = n + 2;
                        if (((Boolean)(isPrime(n)))) {
                            break;
                        }
                    }
                }
            }
        }
        int n_1 = 7;
        int c_1 = 0;
        while (c_1 < 100000) {
            if (((Boolean)(isBrazilian(n_1)))) {
                c_1 = c_1 + 1;
            }
            n_1 = n_1 + 1;
        }
        System.out.println("The 100,000th Brazilian number: " + _p(n_1 - 1));
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

    static String _p(Object v) {
        return v != null ? String.valueOf(v) : "<nil>";
    }
}
