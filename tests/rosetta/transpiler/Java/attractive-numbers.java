public class Main {

    static boolean isPrime(int n) {
        if (n < 2) {
            return false;
        }
        if (((Number)(Math.floorMod(n, 2))).intValue() == 0) {
            return n == 2;
        }
        if (((Number)(Math.floorMod(n, 3))).intValue() == 0) {
            return n == 3;
        }
        int d = 5;
        while (d * d <= n) {
            if (((Number)(Math.floorMod(n, d))).intValue() == 0) {
                return false;
            }
            d = d + 2;
            if (((Number)(Math.floorMod(n, d))).intValue() == 0) {
                return false;
            }
            d = d + 4;
        }
        return true;
    }

    static int countPrimeFactors(int n) {
        if (n == 1) {
            return 0;
        }
        if (isPrime(n)) {
            return 1;
        }
        int count = 0;
        int f = 2;
        while (true) {
            if (((Number)(Math.floorMod(n, f))).intValue() == 0) {
                count = count + 1;
                n = n / f;
                if (n == 1) {
                    return count;
                }
                if (isPrime(n)) {
                    f = n;
                }
            } else             if (f >= 3) {
                f = f + 2;
            } else {
                f = 3;
            }
        }
    }

    static String pad4(int n) {
        String s = String.valueOf(n);
        while (s.length() < 4) {
            s = String.valueOf(" " + s);
        }
        return s;
    }

    static void main() {
        int max = 120;
        System.out.println(String.valueOf("The attractive numbers up to and including " + String.valueOf(max)) + " are:");
        int count = 0;
        String line = "";
        int lineCount = 0;
        int i = 1;
        while (i <= max) {
            int c = countPrimeFactors(i);
            if (isPrime(c)) {
                line = String.valueOf(line + String.valueOf(pad4(i)));
                count = count + 1;
                lineCount = lineCount + 1;
                if (lineCount == 20) {
                    System.out.println(line);
                    line = "";
                    lineCount = 0;
                }
            }
            i = i + 1;
        }
        if (lineCount > 0) {
            System.out.println(line);
        }
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
}
