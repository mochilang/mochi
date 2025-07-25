public class Main {

    static boolean isPrime(int n) {
        if (n < 2) {
            return false;
        }
        if (n % 2 == 0) {
            return n == 2;
        }
        if (n % 3 == 0) {
            return n == 3;
        }
        int d = 5;
        while (d * d <= n) {
            if (n % d == 0) {
                return false;
            }
            d = d + 2;
            if (n % d == 0) {
                return false;
            }
            d = d + 4;
        }
        return true;
    }

    static int sumDigits(int n) {
        int s = 0;
        int x = n;
        while (x > 0) {
            s = s + x % 10;
            x = ((Number)((x / 10))).intValue();
        }
        return s;
    }

    static String pad(int n) {
        if (n < 10) {
            return "  " + String.valueOf(n);
        }
        if (n < 100) {
            return " " + String.valueOf(n);
        }
        return String.valueOf(n);
    }

    static void main() {
        System.out.println("Additive primes less than 500:");
        int count = 0;
        String line = "";
        int lineCount = 0;
        int i = 2;
        while (i < 500) {
            if (isPrime(i) && isPrime(sumDigits(i))) {
                count = count + 1;
                line = String.valueOf(String.valueOf(line + String.valueOf(pad(i))) + "  ");
                lineCount = lineCount + 1;
                if (lineCount == 10) {
                    System.out.println(line.substring(0, line.length() - 2));
                    line = "";
                    lineCount = 0;
                }
            }
            if (i > 2) {
                i = i + 2;
            } else {
                i = i + 1;
            }
        }
        if (lineCount > 0) {
            System.out.println(line.substring(0, line.length() - 2));
        }
        System.out.println(String.valueOf(count) + " additive primes found.");
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
